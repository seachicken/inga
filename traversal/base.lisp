(defpackage #:inga/traversal/base
  (:use #:cl
        #:inga/utils)
  (:import-from #:alexandria
                #:switch)
  (:import-from #:jsown)
  (:import-from #:inga/cache
                #:defunc)
  (:import-from #:inga/file
                #:get-file-type)
  (:import-from #:inga/errors
                #:inga-error)
  (:import-from #:inga/ast-index
                #:ast-index-paths
                #:clean-indexes
                #:create-indexes
                #:get-ast) 
  (:import-from #:inga/logger
                #:log-error)
  (:export #:traversal
           #:*traversals*
           #:*file-index*
           #:*rest-client-apis*
           #:traversal-path
           #:traversal-index
           #:start-traversal
           #:stop-traversal
           #:get-scoped-index-paths
           #:get-scoped-index-paths-generic
           #:set-index-group
           #:find-definitions
           #:find-definitions-generic
           #:find-entrypoint
           #:find-entrypoint-generic
           #:find-references
           #:find-reference
           #:find-rest-clients
           #:find-fq-name
           #:find-fq-name-generic
           #:find-fq-class-name
           #:find-fq-class-name-generic
           #:find-reference-to-literal
           #:find-reference-to-literal-generic
           #:find-signature
           #:find-class-hierarchy
           #:find-class-hierarchy-generic
           #:convert-to-top-offset
           #:convert-to-pos
           #:contains-offset
           #:ast-value
           #:get-asts
           #:filter-by-name
           #:filter-by-names
           #:ast-find-suffix
           #:find-ast
           #:debug-ast))
(in-package #:inga/traversal/base)

(defparameter *index-path* (uiop:merge-pathnames* #p"inga_temp/"))
(defparameter *traversals* nil)
(defparameter *file-index* (make-hash-table))
(defparameter *rest-client-apis* (make-hash-table))

(defclass traversal ()
  ((path
     :initarg :path
     :accessor traversal-path)
   (index
     :initarg :index
     :accessor traversal-index)))

(defgeneric start-traversal (kind include exclude path index)
  (:method (kind include exclude path index)
   (error (format nil "unknown traversal. kind: ~a" kind))))

(defgeneric stop-traversal (traversal)
  (:method (traversal)))

(defmethod stop-traversal :after (traversal)
  (clrhash *file-index*)
  (clrhash *rest-client-apis*))

(defun get-scoped-index-paths (pos index)
  (let ((traversal (get-traversal (cdr (assoc :path (or (cdr (assoc :file-pos pos)) pos))))))
    (if traversal
        (get-scoped-index-paths-generic traversal pos)
        (ast-index-paths index))))
(defgeneric get-scoped-index-paths-generic (traversal pos)
  (:method (traversal pos)
   (ast-index-paths (traversal-index traversal))))

(defgeneric set-index-group (traversal path)
  (:method (traversal path)))

(defun find-definitions (range)
  (funtime
    (lambda ()
      (let ((traversal (get-traversal (cdr (assoc :path range)))))
        (find-definitions-generic traversal range)))
    :label "find-definitions"
    :args range))
(defgeneric find-definitions-generic (traversal range))

(defun find-entrypoint (pos)
  (find-entrypoint-generic (cdr (assoc :typescript *traversals*)) pos))
(defgeneric find-entrypoint-generic (traversal pos)
  (:method (traversal pos)))

(defunc find-references (pos index)
  (funtime
    (lambda ()
      (loop for path in (get-scoped-index-paths pos index)
            with results
            do
            (let* ((traversal (get-traversal path))
                   (ast (get-ast (traversal-index traversal) path))
                   (references (find-references-by-file traversal path ast pos)))
              (when references
                (setf results (append results references))))
            finally (return results)))
    :label "find-references"
    :args pos))

(defun find-references-by-file (traversal path ast target-pos)
  (let ((q (make-queue))
        results)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (when (null ast) (return))

        (let* ((fq-name (find-fq-name ast path))
               (ref (when fq-name (find-reference traversal target-pos fq-name ast path))))
          (when ref
            (setf results (append results (list ref)))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children")
                do (enqueue q child)))))
    results))

(defgeneric find-reference (traversal target-pos fq-name ast path)
  (:method (traversal target-pos fq-name ast path)
   (error (format nil "unknown traversal. path: ~a" path))))

(defgeneric find-rest-clients (traversal fq-name ast path)
  (:method (traversal fq-name ast path)
   (error (format nil "unknown traversal. path: ~a" path))))

(defmethod find-rest-clients :around (traversal fq-name ast path)
  (funtime
    (lambda () (call-next-method))
    :label "find-rest-clients"
    :args path))

(defun find-fq-name (ast path)
  (find-fq-name-generic (get-traversal path) ast path))
(defgeneric find-fq-name-generic (traversal ast path)
  (:method (traversal ast path)
   (error (format nil "unknown traversal. path: ~a" path))))

(defun find-fq-class-name (ast path)
  (find-fq-class-name-generic (get-traversal path) ast path))
(defgeneric find-fq-class-name-generic (traversal ast path))

(defun find-reference-to-literal (ast path)
  (find-reference-to-literal-generic (get-traversal path) ast path))
(defgeneric find-reference-to-literal-generic (traversal ast path)
  (:method (traversal ast path)
   (error (format nil "unknown traversal. path: ~a" path))))

(defun find-signature (fq-name find-signatures index)
  (when (or (null fq-name) (equal fq-name ""))
    (return-from find-signature))

  (let ((fq-class-name
          (format nil "~{~a~^.~}"
                  (butlast (split #\.
                                  (if (>= (length (split #\- fq-name)) 2)
                                      (first (butlast (split #\- fq-name)))
                                      fq-name))))))
    (when (uiop:string-suffix-p fq-class-name "[]")
      (setf fq-class-name (string-right-trim "[]" fq-class-name)))

    (loop for method in (funcall find-signatures fq-class-name)
          with target-name = (subseq (ppcre:regex-replace-all fq-class-name fq-name "") 1)
          with matched-methods
          do
          (when (matches-signature fq-name (cdr (assoc :fq-name method)) index)
            (push method matched-methods))
          finally
          (return (progn
                    (when (> (length matched-methods) 1)
                      (log-error (format nil "get an unexpected ambiguous signature!~%  fq-name: ~a~%  matched-methods: ~a" fq-name matched-methods)))
                    (first matched-methods))))))

(defun matches-signature (target-fq-name api-fq-name index)
  (let* ((split-target-fq-names (split #\- target-fq-name))
         (split-api-fq-names (split #\- api-fq-name))
         (split-api-wild-card-naems (split #\* api-fq-name)))
    (when (uiop:string-prefix-p (first split-api-wild-card-naems) target-fq-name)
      (return-from matches-signature t))

    (unless (equal (first split-target-fq-names) (first split-api-fq-names))
      (return-from matches-signature))

    (let ((target-arg-names (cdr split-target-fq-names))
          (api-arg-names (cdr split-api-fq-names)))
      (unless (or (eq (length target-arg-names) (length api-arg-names))
                  (and (> (length api-arg-names) 0)
                       (is-array (nth (1- (length api-arg-names)) api-arg-names))
                       (or
                         (<= (length api-arg-names) (length target-arg-names))
                         (eq (1- (length api-arg-names)) (length target-arg-names)))))
        (return-from matches-signature))

      (loop for target-arg-name in target-arg-names
            for i below (length target-arg-names)
            with array-arg
            do
            (when (and (< i (length api-arg-names)) (is-array (nth i api-arg-names)))
              (setf array-arg (subseq (nth i api-arg-names) 0 (- (length (nth i api-arg-names)) 2))))
            (unless (or (equal target-arg-name "NULL")
                        (find-if (lambda (super-class-name)
                                   (or
                                     (equal super-class-name (nth i api-arg-names))
                                     (equal super-class-name array-arg)))
                                 (find-class-hierarchy target-arg-name index)))
              (return-from matches-signature)))))
  t)

(defun is-array (name)
  (uiop:string-suffix-p name "[]"))

(defun find-class-hierarchy (fq-class-name index)
  (loop for path in (ast-index-paths index)
        do
        (let* ((traversal (get-traversal (namestring path)))
               (ast (get-ast (traversal-index traversal) path)))
          (let ((class-hierarchy (find-class-hierarchy-generic traversal fq-class-name ast path index)))
            (when class-hierarchy
              (return-from find-class-hierarchy class-hierarchy)))))
  (list fq-class-name))

(defgeneric find-class-hierarchy-generic (traversal fq-class-name root-ast path index)
  (:method (traversal fq-class-name root-ast path index)))

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
                      (cdr (assoc :path pos)))
              :keyword)
      (intern (format nil "refs-~a-~a"
                      (cdr (assoc :path pos))
                      (cdr (assoc :top-offset pos)))
              :keyword)))

(defun get-traversal (path)
  (let ((result (cdr (assoc (get-file-type path) *traversals*))))
    (if result
      result
      (error (format nil "traversal not found. path: ~a" path)))))

(defun contains-offset (a-start a-end b-start b-end)
  (and (<= a-start b-end) (>= a-end b-start)))

(defun ast-value (ast key)
  (and (jsown:keyp ast key)
       (jsown:val ast key)))

(defun get-asts (ast info-path
                     &key (direction :downward)
                     (key-type "type") (key-downward "children") (key-upward "parent"))
  (if (equal direction :horizontal)
      (loop for node in (remove-if (lambda (r) (equal r ast))
                                   (ast-value (ast-value ast key-upward) key-downward))
            with results
            do
            (when (or
                    (find "*" info-path :test #'equal)
                    (find (ast-value node key-type) info-path :test #'equal))
              (push node results))
            finally (return results))
      (loop for path in info-path
            with results = (if (jsown:keyp ast key-type) (list ast) ast)
            do
            (setf results
                  (loop for node in (switch (direction)
                                      (:downward
                                        (apply #'append
                                               (mapcar (lambda (r) (ast-value r key-downward)) results)))
                                      (:upward
                                        (list (ast-value (first results) key-upward))))
                        with nodes
                        do
                        (when (or
                                (equal path "*")
                                (equal path (ast-value node key-type)))
                          (setf nodes (append nodes (list node))))
                        finally (return nodes)))
            finally (return results))))

(defun filter-by-name (nodes name &key (key-name "name"))
  (loop for node in nodes
        with results
        do
        (when (equal (ast-value node key-name) name)
          (setf results (append results (list node))))
        finally (return results)))

(defun filter-by-names (nodes names &key (key-name "name"))
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

(defun find-ast (pos index)
  (loop
    with q = (make-queue)
    with path = (cdr (assoc :path pos))
    with key-offset = (let ((file-type (get-file-type path)))
                        (cond
                          ((eq file-type :java)
                           "pos")
                          ((eq file-type :kotlin)
                           "textOffset")
                          ((t (error (format nil "unexpected file type: ~a" file-type))))))
    with ast 
    initially (enqueue q (get-ast index path))
    do
    (setf ast (dequeue q))
    (when (or (null ast)
              (eq (ast-value ast key-offset) (cdr (assoc :top-offset pos))))
      (return ast))

    (loop for child in (ast-value ast "children") do (enqueue q child))))

(defun debug-ast (ast &key (key-downward "children") (key-upward "parent"))
  (format t "~&ast:~%") 
  (loop for key in (remove-if (lambda (key)
                                (or (equal key key-downward) (equal key key-upward)))
                              (jsown:keywords ast))
        do (format t " (~a . ~a)~%" key (ast-value ast key)))
  ast)

