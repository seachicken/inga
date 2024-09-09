(defpackage #:inga/contexts
  (:use #:cl)
  (:export #:*mode*
           #:make-context
           #:context-kind
           #:context-project-path
           #:context-include
           #:context-exclude
           #:context-lc
           #:context-ast-index
           #:context-analyzers
           #:context-processes))
(in-package #:inga/contexts)

(defparameter *mode* nil)

(defstruct context
  mode
  kind
  project-path
  include
  exclude
  lc
  ast-index
  analyzers
  processes)

