(defpackage #:inga/logger
   (:use #:cl)
   (:export #:log-debug))
(in-package #:inga/logger)

(defparameter *debug* (when (uiop:getenv "INGA_DEBUG") t))

(defun log-debug (content)
  (format t "env debug: ~a, debug: ~a~%" (uiop:getenv "INGA_DEBUG") *debug*)
  (when *debug*
    (format t "~&~a~%" content)))

