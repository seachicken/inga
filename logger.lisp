(defpackage #:inga/logger
  (:nicknames #:logger)
  (:use #:cl)
  (:import-from #:local-time)
  (:export #:log-debug))
(in-package #:inga/logger)

(defun log-debug (content)
  (let ((debug (uiop:getenv "INGA_DEBUG")))
    (when (equal debug "1")
      (format t "~&~a ~a~%" (local-time:now) content))))

