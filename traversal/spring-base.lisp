(defpackage #:inga/traversal/spring-base
  (:use #:cl
        #:inga/traversal/base)
  (:export #:get-value-from-request-mapping
           #:get-value-from-path-variable))
(in-package #:inga/traversal/spring-base)

(defgeneric get-value-from-request-mapping (type ast))

(defgeneric get-value-from-path-variable (type ast))

