(defpackage #:inga/traversal/spring-base
  (:use #:cl
        #:inga/traversal/base)
  (:export #:get-value-from-path-variable))
(in-package #:inga/traversal/spring-base)

(defgeneric get-value-from-path-variable (type ast))

