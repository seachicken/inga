(defpackage #:inga/logger
  (:use #:cl)
  (:import-from #:local-time)
  (:export #:log-debug))
(in-package #:inga/logger)

(defun log-debug (content)
  (when (uiop:getenv "INGA_DEBUG")
    (format t "~&~a ~a~%" (local-time:now) content)))

