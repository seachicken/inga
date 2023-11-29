(defpackage #:inga/ast-analyzer/base
  (:use #:cl
        #:inga/utils)
  (:import-from #:inga/cache
                #:defunc)
  (:import-from #:inga/file
                #:is-analysis-target
                #:get-file-type)
  (:import-from #:inga/errors
                #:inga-error)
  (:import-from #:inga/ast-index
                #:ast-index-paths
                #:clean-indexes
                #:create-indexes
                #:get-ast) 
  (:export #:ast-analyzer
           #:*ast-analyzers*
           #:ast-analyzer-path
           #:ast-analyzer-index
           #:start-ast-analyzer
           #:stop-ast-analyzer
           #:get-scoped-index-paths
           #:get-scoped-index-paths-generic
           #:find-definitions
           #:find-definitions-generic
           #:find-entrypoint
           #:find-entrypoint-generic
           #:find-references
           #:find-reference
           #:find-signature
           #:matches-signature
           #:find-class-hierarchy
           #:find-class-hierarchy-generic
           #:find-package-index-key
           #:find-package-index-key-generic
           #:find-project-index-key
           #:find-project-index-key-generic
           #:convert-to-top-offset
           #:convert-to-pos
           #:contains-offset
           #:ast-value
           #:ast-get
           #:ast-find-name
           #:ast-find-names
           #:ast-find-suffix))
(in-package #:inga/ast-analyzer/base)

(defparameter *index-path* (uiop:merge-pathnames* #p"inga_temp/"))
(defparameter *ast-analyzers* nil)

(defclass ast-analyzer ()
  ((path
     :initarg :path
     :accessor ast-analyzer-path)
   (index
     :initarg :index
     :accessor ast-analyzer-index)))

(defgeneric start-ast-analyzer (kind include exclude path index)
  (:method (kind include exclude path index)
   (error 'unknown-ast-analyzer :name kind)))

(defgeneric stop-ast-analyzer (ast-analyzer)
  (:method (ast-analyzer)))

(defun get-scoped-index-paths (pos index)
  (let ((ast-analyzer (get-ast-analyzer (cdr (assoc :path pos)))))
    (if ast-analyzer
        (get-scoped-index-paths-generic ast-analyzer pos)
        (ast-index-paths index))))
(defgeneric get-scoped-index-paths-generic (ast-analyzer pos)
  (:method (ast-analyzer pos)
   (ast-index-paths (ast-analyzer-index ast-analyzer))))

(defun find-definitions (range)
  (let ((ast-analyzer (get-ast-analyzer (cdr (assoc :path range)))))
    (find-definitions-generic ast-analyzer range)))
(defgeneric find-definitions-generic (ast-analyzer range))

(defun find-entrypoint (pos)
  (find-entrypoint-generic (cdr (assoc :typescript *ast-analyzers*)) pos))
(defgeneric find-entrypoint-generic (ast-analyzer pos)
  (:method (ast-analyzer pos)))

(defunc find-references (pos index)
  (inga/utils::funtime
    (lambda ()
      (loop for path in (get-scoped-index-paths pos index)
            with results
            with ast
            do
            (let ((ast-analyzer (get-ast-analyzer (namestring path))))
              (setf ast (get-ast (ast-analyzer-index ast-analyzer) path))

              (let ((references (find-references-by-file ast-analyzer path ast pos)))
                (when references
                  (setf results (append results references)))))
            finally (return results)))
    :label "find-references"
    :args pos))

(defgeneric find-reference (ast-analyzer target-pos ast path))

(defun find-references-by-file (ast-analyzer path ast target-pos)
  (let ((q (make-queue))
        results)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (when (null ast) (return))

        (let ((ref (find-reference ast-analyzer target-pos ast path)))
          (when ref
            (setf results (append results (list ref)))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children")
                do (enqueue q child)))))
    results))

(defun find-signature (fq-name find-signatures index)
  (when (or (null fq-name) (equal fq-name ""))
    (return-from find-signature))

  (let ((fq-class-name
          (format nil "~{~a~^.~}"
                  (butlast (split #\.
                                  (if (>= (length (split #\- fq-name)) 2)
                                      (first (butlast (split #\- fq-name)))
                                      fq-name))))))
    (loop for method in (funcall find-signatures fq-class-name)
          with target-name = (subseq (ppcre:regex-replace-all fq-class-name fq-name "") 1)
          with matched-methods
          do
          (let ((api-name (format nil "~a~:[~;-~]~:*~{~a~^-~}"
                              (jsown:val method "name")
                              (mapcar (lambda (type) (jsown:val type "name"))
                                      (jsown:val method "parameterTypes")))))
            (when (matches-signature target-name api-name index)
              (push method matched-methods)))
          finally
          (return (if (> (length matched-methods) 1)
                      (loop for method in matched-methods
                            do (unless (jsown:val (jsown:val method "returnType") "isInterface")
                                 (return method)))
                      (first matched-methods))))))

(defun matches-signature (target-fq-name api-fq-name index)
  (let ((split-target-fq-names (split #\- target-fq-name))
        (split-api-fq-names (split #\- api-fq-name)))
    (unless (equal (first split-target-fq-names) (first split-api-fq-names))
      (return-from matches-signature))

    (let ((target-arg-names (cdr split-target-fq-names))
          (api-arg-names (cdr split-api-fq-names)))
      (unless (or (eq (length target-arg-names) (length api-arg-names))
                  (and (> (length api-arg-names) 0)
                       (<= (length api-arg-names) (length target-arg-names))
                       (is-array (nth (1- (length api-arg-names)) api-arg-names))))
        (return-from matches-signature))

      (loop for target-arg-name in target-arg-names
            for i below (length target-arg-names)
            with array-arg
            do
            (when (and (< i (length api-arg-names)) (is-array (nth i api-arg-names)))
              (setf array-arg (subseq (nth i api-arg-names) 0 (- (length (nth i api-arg-names)) 2))))
            (unless (find-if (lambda (super-class-name)
                               (or
                                 (equal super-class-name (nth i api-arg-names))
                                 (equal super-class-name array-arg)))
                             (find-class-hierarchy target-arg-name index))
              (return-from matches-signature)))))
  t)

(defun is-array (name)
  (uiop:string-suffix-p name "[]"))

(defun find-class-hierarchy (fq-class-name index)
  (loop for path in (ast-index-paths index)
        do
        (let* ((ast-analyzer (get-ast-analyzer (namestring path)))
               (ast (get-ast (ast-analyzer-index ast-analyzer) path)))
          (let ((class-hierarchy (find-class-hierarchy-generic ast-analyzer fq-class-name ast path index)))
            (when class-hierarchy
              (return-from find-class-hierarchy class-hierarchy)))))
  (list fq-class-name))

(defgeneric find-class-hierarchy-generic (ast-analyzer fq-class-name root-ast path index)
  (:method (ast-analyzer fq-class-name root-ast path index)))

(defun convert-to-top-offset (path pos)
  (with-open-file (stream path)
    (loop for file-line = (read-line stream nil)
          with line-no = 0
          with top-offset = 0
          while file-line
          when (eq line-no (1- (cdr (assoc :line pos))))
          return (+ top-offset (if (< (cdr (assoc :offset pos)) 0)
                                   (length file-line)
                                   (1- (cdr (assoc :offset pos)))))
          do
          (setf line-no (1+ line-no))
          ;; add newline code
          (setq top-offset (+ top-offset (length file-line) 1)))))

(defun convert-to-pos (path top-offset)
  (with-open-file (stream path)
    (loop for file-line = (read-line stream nil)
          with line-no = 0
          with current-offset = 0
          while file-line
          when (<= top-offset (+ current-offset (length file-line)))
          return (list
                   (cons :line (1+ line-no))
                   (cons :offset (- (1+ (length file-line))
                                    (- (+ current-offset (length file-line)) top-offset))))
          do
          (setf line-no (1+ line-no))
          ;; add newline code
          (setf current-offset (+ current-offset (length file-line) 1)))))

(defun get-references-key (pos)
  (if (eq (cdr (assoc :type pos)) :rest-server)
      (intern (format nil "refs-~a-~a-~a-~a"
                      (cdr (assoc :type pos))
                      (cdr (assoc :host pos))
                      (cdr (assoc :name pos))
                      (cdr (assoc :path pos))))
      (intern (format nil "refs-~a-~a"
                      (cdr (assoc :path pos))
                      (cdr (assoc :top-offset pos))))))

(defun get-ast-analyzer (path)
  (cdr (assoc (get-file-type path) *ast-analyzers*)))

(defun contains-offset (a-start a-end b-start b-end)
  (and (<= a-start b-end) (>= a-end b-start)))

(defun ast-value (ast key)
  (and (jsown:keyp ast key)
       (jsown:val ast key)))

(defun ast-get (ast info-path
                    &key (direction :downward)
                    (key-type "type") (key-downward "children") (key-upward "parent"))
  (loop for path in info-path
        with key-direction = (if (eq direction :downward) key-downward key-upward)
        with results = (if (ast-value ast key-type) (list ast) ast)
        do
        (setf results
              (loop for node in (if (eq direction :downward)
                                    (apply #'append
                                           (mapcar (lambda (r) (ast-value r key-direction)) results))
                                    (list (ast-value (first results) key-direction)))
                    with nodes
                    do
                    (when (or
                            (equal path "*")
                            (equal path (ast-value node key-type)))
                      (setf nodes (append nodes (list node))))
                    finally (return nodes)))
        finally (return results)))

(defun ast-find-name (nodes name &key (key-name "name"))
  (loop for node in nodes
        with results
        do
        (when (equal (ast-value node key-name) name)
          (setf results (append results (list node))))
        finally (return results)))

(defun ast-find-names (nodes names &key (key-name "name"))
  (loop for node in nodes
        with results
        do
        (when (find-if (lambda (n) (equal (ast-value node key-name) n)) names)
          (setf results (append results (list node))))
        finally (return results)))

(defun ast-find-suffix (nodes suffix &key (key-name "name"))
  (loop for node in nodes
        with results
        do
        (when (and
                (jsown:keyp node key-name)
                (uiop:string-suffix-p (jsown:val node key-name) suffix))
          (setf results (append results (list node))))
        finally (return results)))

