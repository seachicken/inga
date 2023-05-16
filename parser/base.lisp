(defpackage #:inga/parser/base
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
  (:export #:parser
           #:*index-path*
           #:make-parser
           #:parser-process
           #:parser-path
           #:parser-cache
           #:start-parser
           #:stop-parser
           #:parse
           #:exec-parser
           #:find-affected-pos
           #:find-entrypoint
           #:find-caller
           #:find-references
           #:matches-reference-name
           #:find-reference-pos
           #:convert-to-top-offset
           #:convert-to-pos
           #:exec-command
           #:get-parse-key
           #:create-indexes
           #:clean-indexes
           #:get-index-path
           #:get-original-path))
(in-package #:inga/parser/base)

(defparameter *index-path* #p"temp/")

(defclass parser ()
  ((process :initform nil
            :accessor parser-process)
   (path :initarg :path
         :accessor parser-path)
   (cache :initarg :cache
          :accessor parser-cache)))

(defgeneric make-parser (kind path cache)
  (:method (kind path cache)
    (error 'unknown-parser :name kind)))

(defgeneric start-parser (parser include exclude))
(defmethod start-parser ((parser list) include exclude)
  (loop for p in parser
        do (start-parser p include exclude)))

(defgeneric stop-parser (parser))
(defmethod stop-parser ((parser list))
  (loop for p in parser
        do (stop-parser p)))

(defgeneric parse (parser path)
  (:method (parser path)
    (exec-command parser path)))
(defmethod parse ((parser list) path)
  (loop for p in parser
    do (let ((p (find-parser parser path)))
         (when p
           (return (parse p path))))))

(defgeneric exec-parser (parser file-path)
  (:method (parser file-path)
    (cdr (jsown:parse (uiop:read-file-string (get-index-path file-path))))))
(defmethod exec-parser ((parser list) file-path)
  (let ((p (find-parser parser file-path)))
    (when p
      (exec-parser p file-path))))

(defgeneric find-affected-pos (parser file-path ast line-no))
(defmethod find-affected-pos ((parser list) file-path ast line-no)
  (let ((p (find-parser parser file-path)))
    (when p
      (find-affected-pos p file-path ast line-no))))

(defgeneric find-entrypoint (parser pos))
(defmethod find-entrypoint ((parser list) pos))

(defgeneric find-caller (parser index-path ast pos)
  (:method (parser index-path ast pos)))

(defgeneric find-references (parser pos)
  (:method (parser pos)
    (let ((cache (get-value (parser-cache (first parser)) (get-references-key pos))))
      (values
        (if (null cache)
            (loop for path in (uiop:directory-files *index-path*)
                  with results
                  with ast
                  with target-parser
                  with cache
                  do (progn
                       (setf target-parser (find-parser parser (namestring path)))
                       (setf ast (cdr (jsown:parse (uiop:read-file-string path))))
                       (let ((callers (find-references-by-file target-parser path ast pos)))
                         (when callers
                           (setf results (append results callers)))))
                  finally (progn
                            (put-value (parser-cache (first parser)) (get-references-key pos)
                                       (if results results 'empty))
                            (return (values results nil))))
            (if (eq cache 'empty) nil cache))
        (when cache t)))))

(defgeneric matches-reference-name (parser ast target-name))

(defgeneric find-reference-pos (parser index-path root-ast ast target-pos))

(defun find-references-by-file (parser index-path ast target-pos)
  (let ((q (make-queue))
        (target-name (cdr (assoc :name target-pos)))
        (root-ast ast)
        results)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (matches-reference-name parser ast target-name)
          (let ((found-reference-pos (find-reference-pos parser index-path root-ast ast target-pos)))
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

(defun exec-command (parser command)
  (write-line command (uiop:process-info-input (parser-process parser)))
  (force-output (uiop:process-info-input (parser-process parser)))
  (read-line (uiop:process-info-output (parser-process parser))))

(defun get-parse-key (path)
  (intern (format nil "parse-~a" path)))

(defun get-references-key (pos)
  (intern (format nil "refs-~a-~a-~a" (cdr (assoc :path pos)) (cdr (assoc :line pos)) (cdr (assoc :offset pos)))))

(defmethod find-parser (parser file-path)
  parser)
(defmethod find-parser ((parsers list) file-path)
  (loop for p in parsers
        with type = (get-file-type file-path)
        do (when (or (and (string= (string (type-of p)) "PARSER-JAVA") (eq type 'java))
                     (and (string= (string (type-of p)) "PARSER-KOTLIN") (eq type 'kotlin)))
             (return p))))

(defun get-file-type (path)
  (if (is-match path '("*.java"))
      'java
      (when (is-match path '("*.kt"))
        'kotlin)))

(defun create-indexes (parser include exclude)
  (ensure-directories-exist *index-path*) 
  (loop for path in (uiop:directory-files (format nil "~a/**/*" (parser-path parser)))
        do (let ((relative-path (enough-namestring path (parser-path parser))))
             (when (is-analysis-target relative-path include exclude)
               (handler-case
                 (alexandria:write-string-into-file
                   (format nil "~a" (parse parser (namestring path)))
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
          (subseq (split #\/ (ppcre:regex-replace-all
                               "--"
                               (enough-namestring index-path)
                               "/"))
                  1)))  

