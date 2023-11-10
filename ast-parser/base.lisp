(defpackage #:inga/ast-parser/base
  (:use #:cl)
  (:import-from #:jsown)
  (:import-from #:inga/file
                #:get-file-type)
  (:export #:ast-parser
           #:ast-parser-process
           #:start
           #:stop
           #:stop-all-parsers
           #:parse))
(in-package #:inga/ast-parser/base)

(defparameter *ast-parsers* nil)

(defclass ast-parser ()
  ((process
     :initarg :process
     :accessor ast-parser-process)))

(defgeneric start (file-type))

(defgeneric stop (ast-parser))

(defun stop-all-parsers ()
  (loop for p in *ast-parsers* do (stop (cdr p)))
  (setf *ast-parsers* nil))

(defun parse (path)
  (run-parser (get-parser path) (namestring path)))

(defun get-parser (path)
  (let ((file-type (get-file-type path)))
    (unless (assoc file-type *ast-parsers*)
      (setf *ast-parsers* (acons file-type (start file-type) *ast-parsers*)))
    (cdr (assoc file-type *ast-parsers*))))

(defun run-parser (ast-parser command)
  (write-line command (uiop:process-info-input (ast-parser-process ast-parser)))
  (force-output (uiop:process-info-input (ast-parser-process ast-parser)))
  (read-line (uiop:process-info-output (ast-parser-process ast-parser))))

