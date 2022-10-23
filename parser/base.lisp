(defpackage #:inga/parser/base
  (:use #:cl)
  (:export #:parser
           #:make-parser
           #:parser-process
           #:parser-path
           #:start-parser
           #:stop-parser
           #:exec-parser
           #:find-affected-pos
           #:count-combinations
           #:find-entrypoint
           #:exec-command))
(in-package #:inga/parser/base)

(defclass parser ()
  ((process :initform nil
            :accessor parser-process)
   (path :initarg :path
         :accessor parser-path)))

(defgeneric make-parser (kind path)
  (:method (kind path)
    (error 'unknown-parser :name kind)))

(defgeneric start-parser (parser))

(defgeneric stop-parser (parser))

(defgeneric exec-parser (parser file-path))

(defgeneric find-affected-pos (parser file-path ast line-no))

(defgeneric count-combinations (parser file-path ast line-nos))

(defgeneric find-entrypoint (parser pos))

(defun exec-command (parser command)
  (write-line command (uiop:process-info-input (parser-process parser)))
  (force-output (uiop:process-info-input (parser-process parser)))
  (read-line (uiop:process-info-output (parser-process parser))))

