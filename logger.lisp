(defpackage #:inga/logger
  (:nicknames #:logger)
  (:use #:cl)
  (:import-from #:local-time)
  (:import-from #:inga/context
                #:*mode*)
  (:export #:log-debug
           #:log-debug-generic
           #:log-info
           #:log-info-generic
           #:log-error
           #:log-error-generic))
(in-package #:inga/logger)

(defun log-debug (content)
  (let ((debug (uiop:getenv "INGA_DEBUG")))
    (when (equal debug "1")
      (log-debug-generic *mode* content))))
(defgeneric log-debug-generic (mode content)
  (:method (mode content)
   (format t "~&~a DEBUG ~a~%" (local-time:now) content)))

(defun log-info (content)
  (log-info-generic *mode* content))
(defgeneric log-info-generic (mode content)
  (:method (mode content)
   (format t "~&~a INFO ~a~%" (local-time:now) content)))

(defun log-error (content)
  (log-error-generic *mode* content))
(defgeneric log-error-generic (mode content)
  (:method (mode content)
   (format *error-output* "~&~a ERROR ~a~%" (local-time:now) content)))

