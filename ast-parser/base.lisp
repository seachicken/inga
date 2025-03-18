(defpackage #:inga/ast-parser/base
  (:use #:cl)
  (:import-from #:jsown)
  (:import-from #:inga/file
                #:get-file-type)
  (:import-from #:inga/logger
                #:log-error)
  (:export #:ast-parser
           #:ast-parser-process
           #:start
           #:stop
           #:parse
           #:parse-generic
           #:version
           #:version-generic
           #:stop-all-parsers))
(in-package #:inga/ast-parser/base)

(defparameter *ast-parsers* nil)

(defclass ast-parser ()
  ((process
     :initarg :process
     :accessor ast-parser-process)))

(defgeneric start (file-type))

(defgeneric stop (ast-parser))

(defun parse (path)
  (parse-generic (get-parser path) path))

(defgeneric parse-generic (ast-parser path)
  (:method (ast-parser path)
   (let ((command (namestring path)))
     (write-line command (uiop:process-info-input (ast-parser-process ast-parser)))
     (force-output (uiop:process-info-input (ast-parser-process ast-parser)))
     (prog1
       (read-line (uiop:process-info-output (ast-parser-process ast-parser)) nil nil) 
       (loop while (listen (uiop:process-info-error-output (ast-parser-process ast-parser)))
             with results = ""
             do (setf results
                      (format nil "~a~a~%"
                              results 
                              (read-line (uiop:process-info-error-output
                                           (ast-parser-process ast-parser)))))
             finally (unless (equal results "")
                       (log-error (format nil "~a, cmd: ~a" results command))))))))

(defun version (path)
  (version-generic (get-parser path)))
(defgeneric version-generic (ast-parser))

(defun stop-all-parsers ()
  (loop for p in *ast-parsers* do (stop (cdr p)))
  (setf *ast-parsers* nil))

(defun get-parser (path)
  (let ((file-type (get-file-type path)))
    (unless (and (assoc file-type *ast-parsers*)
                 (uiop:process-alive-p (ast-parser-process (cdr (assoc file-type *ast-parsers*)))))
      (setf *ast-parsers* (acons file-type (start file-type) *ast-parsers*)))
    (cdr (assoc file-type *ast-parsers*))))

