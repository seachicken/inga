(defpackage #:inga/ast-analyzer/base
  (:use #:cl
        #:inga/utils)
  (:import-from #:inga/file
                #:is-match
                #:is-analysis-target)
  (:import-from #:inga/cache
                #:put-value
                #:get-value)
  (:import-from #:inga/errors
                #:inga-error)
  (:export #:ast-analyzer
           #:*ast-analyzers*
           #:ast-analyzer-process
           #:ast-analyzer-path
           #:ast-analyzer-cache
           #:start-ast-analyzer
           #:stop-ast-analyzer
           #:find-definitions
           #:find-definitions-generic
           #:find-entrypoint
           #:find-entrypoint-generic
           #:find-references
           #:find-reference
           #:find-signature
           #:find-signatures
           #:find-signatures-generic
           #:convert-to-top-offset
           #:convert-to-pos
           #:exec-command
           #:create-indexes
           #:clean-indexes
           #:get-index-path
           #:get-original-path
           #:contains-offset
           #:ast-value
           #:ast-get
           #:ast-find-name
           #:ast-find-names
           #:ast-find-suffix))
(in-package #:inga/ast-analyzer/base)

(defparameter *index-path* (uiop:merge-pathnames* #p"inga_temp/"))
(defparameter *ast-analyzers* nil)
(defvar *cache*)

(defclass ast-analyzer ()
  ((process
     :initarg :process
     :accessor ast-analyzer-process)
   (path
     :initarg :path
     :accessor ast-analyzer-path)))

(defgeneric start-ast-analyzer (kind exclude path)
  (:method (kind exclude path)
    (error 'unknown-ast-analyzer :name kind)))

(defgeneric stop-ast-analyzer (ast-analyzer))

(defun find-definitions (range)
  (let ((ast-analyzer (get-ast-analyzer (cdr (assoc :path range)))))
    (find-definitions-generic ast-analyzer range)))
(defgeneric find-definitions-generic (ast-analyzer range))

(defun find-entrypoint (pos)
  (find-entrypoint-generic (cdr (assoc :typescript *ast-analyzers*)) pos))
(defgeneric find-entrypoint-generic (ast-analyzer pos)
  (:method (ast-analyzer pos)))

(defun find-references (pos)
  (let ((cached-value (get-value *cache* (get-references-key pos))))
    (values
      (if (null cached-value)
          (loop for path in (uiop:directory-files *index-path*)
                with results
                with ast
                do
                (let ((ast-analyzer (get-ast-analyzer (namestring path))))
                  (handler-case
                    (setf ast (jsown:parse (alexandria:read-file-into-string path)))
                    (error (e)
                           (format t "~a~%" e)
                           (return (values results nil))))

                  (let ((references (find-references-by-file ast-analyzer path ast pos)))
                    (when references
                      (setf results (append results references)))))
                finally
                (put-value *cache* (get-references-key pos)
                           (if results results 'empty))
                (return (values results nil)))
          (if (eq cached-value 'empty) nil cached-value))
      (when cached-value t))))

(defgeneric find-reference (ast-analyzer target-pos ast index-path))

(defun find-references-by-file (ast-analyzer index-path ast target-pos)
  (let ((q (make-queue)))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (when (null ast) (return))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children")
                do
                (setf (jsown:val child "parent") ast)
                (enqueue q child))))))
  (let ((q (make-queue))
        results)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (when (null ast) (return))

        (let ((ref (find-reference ast-analyzer target-pos ast index-path)))
          (when ref
            (setf results (append results (list ref)))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children")
                do (enqueue q child)))))
    results))

(defun find-signature (fq-name find-signatures &optional from)
  (when (or (null fq-name) (equal fq-name ""))
    (return-from find-signature))

  (let ((fq-class-name
          (format nil "~{~a~^.~}"
                  (butlast (split #\.
                                  (if (>= (length (split #\- fq-name)) 2)
                                      (first (butlast (split #\- fq-name)))
                                      fq-name))))))
    (loop for method in (funcall find-signatures fq-class-name from)
          with target-name = (subseq (ppcre:regex-replace-all fq-class-name fq-name "") 1)
          with matched-methods
          do
          (let ((name (format nil "~a~:[~;-~]~:*~{~a~^-~}"
                              (jsown:val method "name")
                              (if (equal (ast-value method "kind") "variable")
                                  nil
                                  (mapcar (lambda (type) (jsown:val type "name"))
                                          (jsown:val method "parameterTypes"))))))
            (when (equal name target-name)
              (push method matched-methods)))
          finally
          (return (if (> (length matched-methods) 1)
                      (loop for method in matched-methods
                            do (unless (jsown:val (jsown:val method "returnType") "isInterface")
                                 (return method)))
                      (first matched-methods))))))

(defun find-signatures (fq-class-name &optional from)
  (loop for path in (uiop:directory-files *index-path*)
      do
      (let ((ast-analyzer (get-ast-analyzer (namestring path)))
            (ast (jsown:parse (alexandria:read-file-into-string path))))
        (let ((signatures (find-signatures-generic ast-analyzer fq-class-name ast)))
          (when signatures
            (return-from find-signatures signatures))))))

(defgeneric find-signatures-generic (ast-analyzer fq-class-name root-ast)
  (:method (ast-analyzer fq-class-name root-ast)))

(defun convert-to-top-offset (root-path path pos)
  (with-open-file (stream (uiop:merge-pathnames* path root-path))
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

(defun convert-to-pos (root-path path top-offset)
  (with-open-file (stream (uiop:merge-pathnames* path root-path))
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

(defun exec-command (ast-analyzer command)
  (write-line command (uiop:process-info-input (ast-analyzer-process ast-analyzer)))
  (force-output (uiop:process-info-input (ast-analyzer-process ast-analyzer)))
  (read-line (uiop:process-info-output (ast-analyzer-process ast-analyzer))))

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

(defun get-file-type (path)
  (cond
    ((is-match path '("*.java"))
      :java)
    ((is-match path '("*.kt"))
     :kotlin)
    ((is-match path '("*.(js|jsx)" "*.(ts|tsx)"))
     :typescript)))

(defun create-indexes (root-path &key include exclude)
  (clean-indexes)
  (ensure-directories-exist *index-path*) 
  (loop for path in (uiop:directory-files (format nil "~a/**/*" root-path))
        do (let ((relative-path (enough-namestring path root-path)))
             (when (is-analysis-target relative-path include exclude)
               (handler-case
                 (alexandria:write-string-into-file
                   (format nil "~a" (exec-command (get-ast-analyzer relative-path) (namestring path)))
                   (get-index-path relative-path))
                 (error (e)
                        (format t "error: ~a, path: ~a~%" e path)
                        (error 'inga-error)))))))

(defun clean-indexes ()
  (uiop:delete-directory-tree *index-path*
                              :validate t
                              :if-does-not-exist :ignore))

(defun get-index-path (original-path)
  (uiop:merge-pathnames*
    *index-path*
    (ppcre:regex-replace-all "/" original-path "--")))

(defun get-original-path (index-path)
  (format nil "~{~a~^/~}"
          (split #\/ (ppcre:regex-replace-all
                       "--"
                       (enough-namestring index-path *index-path*)
                       "/"))))

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
        with results = (list ast)
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

