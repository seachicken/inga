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
           #:find-fq-method-name
           #:find-references
           #:convert-to-ast-pos
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

(defgeneric find-fq-method-name (parser src-path root-ast ast result))

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
                       (let ((callers (find-caller target-parser path ast pos)))
                         (when callers
                           (setf results (append results callers)))))
                  finally (progn
                            (put-value (parser-cache (first parser)) (get-references-key pos)
                                       (if results results 'empty))
                            (return (values results nil))))
            (if (eq cache 'empty) nil cache))
        (when cache t)))))

(defun convert-to-ast-pos (project-path pos)
  (let ((path (uiop:merge-pathnames* (cdr (assoc :path pos)) project-path))
        (offset (cdr (assoc :offset pos)))
        (line-no 0)
        (result 0))
    (with-open-file (stream path)
      (loop for line = (read-line stream nil)
            while line
            when (= line-no (- (cdr (assoc :line pos)) 1))
            return (list
                     (cons :path (enough-namestring path project-path))
                     (cons :pos (+ result (if (< offset 0)
                                              (length line)
                                              (- offset 1)))))
            do
            (setq line-no (+ line-no 1))
            ;; add newline code
            (setq result (+ result (+ (length line) 1)))))))

(defun convert-to-pos (project-path path name fq-name pos)
  (let ((line-no 0)
        (cnt 0))
    (with-open-file (stream (uiop:merge-pathnames* path project-path))
      (loop for line = (read-line stream nil)
            while line
            when (<= pos (+ cnt (length line)))
            return (let ((results
                           (list
                             (cons :path (enough-namestring path project-path))
                             (cons :name name)
                             (cons :line (+ line-no 1))
                             (cons :offset (- (+ (length line) 1) (- (+ cnt (length line)) pos))))))
                     (when fq-name
                       (setf results (append results (list (cons :fq-name fq-name)))))
                     results)
            do
              (setq line-no (+ line-no 1))
              ;; add newline code
              (setq cnt (+ cnt (length line) 1))))))

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

