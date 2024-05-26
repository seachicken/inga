(defpackage #:inga/logger
  (:nicknames #:logger)
  (:use #:cl)
  (:import-from #:local-time)
  (:export #:log-debug
           #:log-error))
(in-package #:inga/logger)

(defun log-debug (content)
  (let ((debug (uiop:getenv "INGA_DEBUG")))
    (when (equal debug "1")
      (format t "~&~a ~a~%" (local-time:now) content))))

(defun log-error (content)
  (format *error-output* "~&~a ~a~%" (local-time:now) content))

