(defpackage #:inga/context
  (:use #:cl)
  (:export #:*default-mode*
           #:*mode*))
(in-package #:inga/context)

(defparameter *default-mode* :cli)
(defparameter *mode* *default-mode*)
