(defpackage #:inga/ast-analyzer/base
  (:use #:cl
        #:inga/utils)
  (:import-from #:inga/cache
                #:defunc)
  (:import-from #:inga/file
                #:is-match
                #:is-analysis-target)
  (:import-from #:inga/errors
                #:inga-error)
  (:export #:ast-analyzer
           #:*ast-analyzers*
           #:ast-analyzer-process
           #:ast-analyzer-path
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
           #:matches-signature
           #:find-class-hierarchy
           #:find-class-hierarchy-generic
           #:find-package-index-key-generic
           #:find-project-index-key-generic
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
(defparameter *package-index-groups* nil)
(defparameter *project-index-groups* nil)
(defparameter *ast-analyzers* nil)

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

(defunc find-references (pos)
  (inga/utils::funtime
    (lambda ()
      (loop for path in (get-scoped-index-paths pos)
            with results
            with ast
            do
            (let ((ast-analyzer (get-ast-analyzer (namestring path))))
              (setf ast (parse-to-ast path))

              (let ((references (find-references-by-file ast-analyzer path ast pos)))
                (when references
                  (setf results (append results references)))))
            finally (return results)))
    :label "find-references"
    :args pos))

(defun get-scoped-index-paths (pos)
  (cond
    ((eq (cdr (assoc :type pos)) :module-private)
     (list
       (merge-pathnames
         (get-index-path (cdr (assoc :path pos)))
         (ast-analyzer-path (get-ast-analyzer (cdr (assoc :path pos)))))))
    ((eq (cdr (assoc :type pos)) :module-default)
     (let ((index-path (get-index-path (cdr (assoc :path pos)))))
       (cdr (assoc
              (find-package-index-key (parse-to-ast index-path) index-path)
              *package-index-groups*))))
    ((eq (cdr (assoc :type pos)) :module-public)
     (cdr (assoc
            (find-project-index-key
              (merge-pathnames (cdr (assoc :path pos))
                               (ast-analyzer-path (get-ast-analyzer (cdr (assoc :path pos))))))
            *project-index-groups*)))
    (t
     (uiop:directory-files *index-path*))))

(defgeneric find-reference (ast-analyzer target-pos ast index-path))

(defun find-references-by-file (ast-analyzer index-path ast target-pos)
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

(defun find-signature (fq-name find-signatures)
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
          (let ((name (format nil "~a~:[~;-~]~:*~{~a~^-~}"
                              (jsown:val method "name")
                              (if (equal (ast-value method "kind") "variable")
                                  nil
                                  (mapcar (lambda (type) (jsown:val type "name"))
                                          (jsown:val method "parameterTypes"))))))
            (when (matches-signature target-name name)
              (push method matched-methods)))
          finally
          (return (if (> (length matched-methods) 1)
                      (loop for method in matched-methods
                            do (unless (jsown:val (jsown:val method "returnType") "isInterface")
                                 (return method)))
                      (first matched-methods))))))

(defun find-signatures (fq-class-name)
  (loop for path in (uiop:directory-files *index-path*)
      do
      (let ((ast-analyzer (get-ast-analyzer (namestring path)))
            (ast (parse-to-ast path)))
        (let ((signatures (find-signatures-generic ast-analyzer fq-class-name ast)))
          (when signatures
            (return-from find-signatures signatures))))))

(defgeneric find-signatures-generic (ast-analyzer fq-class-name root-ast)
  (:method (ast-analyzer fq-class-name root-ast)))

(defun matches-signature (target-fq-name api-fq-name)
  (let ((split-target-fq-names (split #\- target-fq-name))
        (split-api-fq-names (split #\- api-fq-name)))
    (unless (equal (first split-target-fq-names) (first split-api-fq-names))
      (return-from matches-signature))

    (let ((target-arg-names (cdr split-target-fq-names))
          (api-arg-names (mapcar (lambda (n)
                                   (cl-ppcre:regex-replace-all "\\[L|;" n ""))
                                 (cdr split-api-fq-names))))
      (loop for target-arg-name in target-arg-names
            for i below (length target-arg-names)
            do
            (unless (find-if (lambda (super-class-name)
                               (equal super-class-name (nth i api-arg-names)))
                             (find-class-hierarchy target-arg-name))
              (return-from matches-signature)))))
  t)

(defun find-class-hierarchy (fq-class-name)
  (loop for path in (uiop:directory-files *index-path*)
      do
      (let ((ast-analyzer (get-ast-analyzer (namestring path)))
            (ast (parse-to-ast path)))
        (let ((class-hierarchy (find-class-hierarchy-generic ast-analyzer fq-class-name ast path)))
          (when class-hierarchy
            (return-from find-class-hierarchy class-hierarchy)))))
  (list fq-class-name))

(defgeneric find-class-hierarchy-generic (ast-analyzer fq-class-name root-ast index-path)
  (:method (ast-analyzer fq-class-name root-ast index-path)))

(defun find-package-index-key (ast index-path)
  (find-package-index-key-generic (get-ast-analyzer (namestring index-path)) ast))
(defgeneric find-package-index-key-generic (ast-analyzer ast)
  (:method (ast-analyzer ast)))

(defun find-project-index-key (path)
  (find-project-index-key-generic (get-ast-analyzer (namestring path)) path))
(defgeneric find-project-index-key-generic (ast-analyzer path)
  (:method (ast-analyzer path)))

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
        do
        (let ((relative-path (enough-namestring path root-path)))
          (when (is-analysis-target relative-path include exclude)
            (handler-case
              (let ((ast (exec-command (get-ast-analyzer relative-path) (namestring path)))
                    (index-path (get-index-path relative-path)))
                (alexandria:write-string-into-file (format nil "~a" ast) index-path)

                (let ((index-key (find-package-index-key (jsown:parse ast) index-path)))
                  (setf *package-index-groups*
                        (if (assoc index-key *package-index-groups*)
                            (acons index-key
                                   (append (list index-path) (cdr (assoc index-key *package-index-groups*)))
                                   *package-index-groups*)
                            (acons index-key (list index-path) *package-index-groups*))))

                (let ((index-key (find-project-index-key path)))
                  (setf *project-index-groups*
                        (if (assoc index-key *project-index-groups*)
                            (acons index-key
                                   (append (list index-path) (cdr (assoc index-key *project-index-groups*)))
                                   *project-index-groups*)
                            (acons index-key (list index-path) *project-index-groups*))))) 
              (error (e)
                     (format t "error: ~a, path: ~a~%" e path)
                     (error 'inga-error)))))))

(defun clean-indexes ()
  (setf *package-index-groups* nil)
  (setf *project-index-groups* nil)
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

(defun parse-to-ast (path)
  (let ((ast (jsown:parse (alexandria:read-file-into-string path))))
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
    ast))

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

