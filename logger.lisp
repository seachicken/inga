(defpackage #:inga/logger
   (:use #:cl)
   (:export #:log-debug))
(in-package #:inga/logger)

(defparameter *debug* (when (uiop:getenv "INGA_DEBUG") t))

(defun log-debug (content)
  (when *debug*
    (format t "~&~a~%" content)))

