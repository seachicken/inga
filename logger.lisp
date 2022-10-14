(defpackage #:inga/logger
   (:use #:cl)
   (:export #:log-debug))
(in-package #:inga/logger)

(defun log-debug (content)
  (when (uiop:getenv "INGA_DEBUG")
    (format t "~&~a~%" content)))

