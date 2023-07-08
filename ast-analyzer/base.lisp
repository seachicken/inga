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
           #:*index-path*
           #:make-ast-analyzer
           #:ast-analyzer-process
           #:ast-analyzer-path
           #:ast-analyzer-cache
           #:start-ast-analyzer
           #:stop-ast-analyzer
           #:find-definitions
           #:find-entrypoint
           #:find-caller
           #:find-references
           #:matches-reference-name
           #:find-reference-pos
           #:convert-to-top-offset
           #:convert-to-pos
           #:exec-command
           #:create-indexes
           #:clean-indexes
           #:get-index-path
           #:get-original-path
           #:contains-offset))
(in-package #:inga/ast-analyzer/base)

(defparameter *index-path* (uiop:merge-pathnames* #p"inga_temp/"))

(defclass ast-analyzer ()
  ((process :initform nil
            :accessor ast-analyzer-process)
   (path :initarg :path
         :accessor ast-analyzer-path)
   (cache :initarg :cache
          :accessor ast-analyzer-cache)))

(defgeneric make-ast-analyzer (kind path cache)
  (:method (kind path cache)
    (error 'unknown-ast-analyzer :name kind)))

(defgeneric start-ast-analyzer (ast-analyzer include exclude))
(defmethod start-ast-analyzer ((ast-analyzer list) include exclude)
  (loop for a in ast-analyzer
        do (start-ast-analyzer a include exclude)))

(defgeneric stop-ast-analyzer (ast-analyzer))
(defmethod stop-ast-analyzer ((ast-analyzer list))
  (loop for a in ast-analyzer
        do (stop-ast-analyzer a)))

(defgeneric find-definitions (ast-analyzer range))
(defmethod find-definitions ((ast-analyzer list) range)
  (let ((a (find-ast-analyzer ast-analyzer (cdr (assoc :path range)))))
    (when a
      (find-definitions a range))))

(defgeneric find-entrypoint (ast-analyzer pos))
(defmethod find-entrypoint ((ast-analyzer list) pos))

(defgeneric find-caller (ast-analyzer index-path ast pos)
  (:method (ast-analyzer index-path ast pos)))

(defgeneric find-references (ast-analyzer pos)
  (:method (ast-analyzer pos)
    (let ((cache (get-value (ast-analyzer-cache (first ast-analyzer)) (get-references-key pos))))
      (values
        (if (null cache)
            (loop for path in (uiop:directory-files *index-path*)
                  with results
                  with ast
                  with target-ast-analyzer
                  with cache
                  do
                  (setf target-ast-analyzer (find-ast-analyzer ast-analyzer (namestring path)))
                  (handler-case
                    (setf ast (cdr (jsown:parse (alexandria:read-file-into-string path))))
                    (error (e)
                           (format t "~a~%" e)
                           (return (values results nil))))

                  (let ((references (find-references-by-file target-ast-analyzer path ast pos)))
                    (when references
                      (setf results (append results references))))
                  finally
                  (put-value (ast-analyzer-cache (first ast-analyzer)) (get-references-key pos)
                             (if results results 'empty))
                  (return (values results nil)))
            (if (eq cache 'empty) nil cache))
        (when cache t)))))

(defgeneric matches-reference-name (ast-analyzer ast target-name))

(defgeneric find-reference-pos (ast-analyzer index-path root-ast ast target-pos))

(defun find-references-by-file (ast-analyzer index-path ast target-pos)
  (let ((q (make-queue))
        (target-name (cdr (assoc :name target-pos)))
        (root-ast ast)
        results)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (when (null ast) (return))

        (when (matches-reference-name ast-analyzer ast target-name)
          (let ((found-reference-pos (find-reference-pos ast-analyzer index-path root-ast ast target-pos)))
            (when found-reference-pos
              (setf results (append results (list found-reference-pos))))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children")
                do
                (let ((body (cdr child)))
                  (setf (jsown:val child "parent") ast)
                  (enqueue q (cdr child)))))))
    results))

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
  (intern (format nil "refs-~a-~a" (cdr (assoc :path pos)) (cdr (assoc :top-offset pos)))))

(defmethod find-ast-analyzer (ast-analyzer file-path)
  ast-analyzer)
(defmethod find-ast-analyzer ((ast-analyzers list) file-path)
  (loop for a in ast-analyzers
        with type = (get-file-type file-path)
        do (when (or (and (string= (string (type-of a)) "AST-ANALYZER-JAVA") (eq type 'java))
                     (and (string= (string (type-of a)) "AST-ANALYZER-KOTLIN") (eq type 'kotlin)))
             (return a))))

(defun get-file-type (path)
  (if (is-match path '("*.java"))
      'java
      (when (is-match path '("*.kt"))
        'kotlin)))

(defun create-indexes (ast-analyzer include exclude)
  (ensure-directories-exist *index-path*) 
  (loop for path in (uiop:directory-files (format nil "~a/**/*" (ast-analyzer-path ast-analyzer)))
        do (let ((relative-path (enough-namestring path (ast-analyzer-path ast-analyzer))))
             (when (is-analysis-target relative-path include exclude)
               (handler-case
                 (alexandria:write-string-into-file
                   (format nil "~a" (exec-command ast-analyzer (namestring path)))
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

