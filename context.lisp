(defpackage #:inga/context
  (:use #:cl)
  (:export #:*default-mode*
           #:*mode*
           #:context))
(in-package #:inga/context)

(defparameter *default-mode* :cli)
(defparameter *mode* *default-mode*)

(defstruct context
  kind
  project-path
  include
  exclude
  lc
  ast-index
  analyzers
  processes)

