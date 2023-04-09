(defpackage #:inga/parser/base
  (:use #:cl)
  (:import-from #:inga/file
                #:is-match)
  (:export #:parser
           #:make-parser
           #:parser-process
           #:parser-path
           #:parser-cache
           #:start-parser
           #:stop-parser
           #:exec-parser
           #:find-affected-pos
           #:find-entrypoint
           #:convert-to-ast-pos
           #:convert-to-pos
           #:exec-command
           #:get-parse-key))
(in-package #:inga/parser/base)

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

(defgeneric start-parser (parser))
(defmethod start-parser ((parser list))
  (loop for p in parser
        do (start-parser p)))

(defgeneric stop-parser (parser))
(defmethod stop-parser ((parser list))
  (loop for p in parser
        do (stop-parser p)))

(defgeneric exec-parser (parser file-path))
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
                     (cons :path (pathname path))
                     (cons :pos (+ result (if (< offset 0)
                                              (length line)
                                              (- offset 1)))))
            do
            (setq line-no (+ line-no 1))
            ;; add newline code
            (setq result (+ result (+ (length line) 1)))))))

(defun convert-to-pos (project-path path name pos)
  (let ((line-no 0)
        (cnt 0))
    (with-open-file (stream (uiop:merge-pathnames* path project-path))
      (loop for line = (read-line stream nil)
            while line
            when (<= pos (+ cnt (length line)))
            return (list
                     (cons :path (enough-namestring path project-path))
                     (cons :name name)
                     (cons :line (+ line-no 1))
                     (cons :offset (- (+ (length line) 1) (- (+ cnt (length line)) pos))))
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

(defun find-parser (parsers file-path)
  (loop for p in parsers
        with type = (get-file-type file-path)
        when (or (and (string= (string (type-of p)) "PARSER-JAVA") (eq type 'java))
                 (and (string= (string (type-of p)) "PARSER-KOTLIN") (eq type 'kotlin)))
        when t
        return p))

(defun get-file-type (path)
  (if (is-match path '("*.java"))
      'java
      (when (is-match path '("*.kt"))
        'kotlin)))

