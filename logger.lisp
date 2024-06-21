(defpackage #:inga/logger
  (:nicknames #:logger)
  (:use #:cl)
  (:import-from #:local-time)
  (:import-from #:inga/context
                #:*mode*)
  (:export #:log-debug
           #:log-error
           #:log-error-generic))
(in-package #:inga/logger)

(defun log-debug (content)
  (let ((debug (uiop:getenv "INGA_DEBUG")))
    (when (equal debug "1")
      (format t "~&~a ~a~%" (local-time:now) content))))

(defun log-error (content)
  (log-error-generic *mode* content))
(defgeneric log-error-generic (mode content)
  (:method (mode content)
   (format *error-output* "~&~a ~a~%" (local-time:now) content)))

