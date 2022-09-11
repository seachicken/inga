(defpackage #:inga/parser/base
  (:use #:cl)
  (:export #:parser
           #:make-parser
           #:parser-process
           #:start-parser
           #:stop-parser
           #:exec-parser
           #:find-affected-pos
           #:find-entrypoint
           #:exec-command))
(in-package #:inga/parser/base)

(defclass parser ()
  ((process :initform nil
            :accessor parser-process)))

(defgeneric make-parser (kind)
  (:method (kind)
    (error 'unknown-parser :name kind)))

(defgeneric start-parser (parser))

(defgeneric stop-parser (parser))

(defgeneric exec-parser (parser file-path))

(defgeneric find-affected-pos (parser project-path file-path ast line-no))

(defgeneric find-entrypoint (parser project-path pos))

(defun exec-command (parser command)
  (write-line command (uiop:process-info-input (parser-process parser)))
  (force-output (uiop:process-info-input (parser-process parser)))
  (read-line (uiop:process-info-output (parser-process parser))))

