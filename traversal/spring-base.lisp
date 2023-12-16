(defpackage #:inga/traversal/spring-base
  (:use #:cl
        #:inga/traversal/base)
  (:import-from #:alexandria
                #:switch)
  (:export #:get-values-from-request-mapping
           #:get-method-from-request-mapping
           #:get-value-from-path-variable
           #:to-http-method))
(in-package #:inga/traversal/spring-base)

(defgeneric get-values-from-request-mapping (type ast))

(defgeneric get-method-from-request-mapping (type ast))

(defgeneric get-value-from-path-variable (type ast))

(defun to-http-method (type)
  (switch (type :test #'equal)
    ("GetMapping" "GET")
    ("PostMapping" "POST")  
    ("PutMapping" "PUT")
    ("DeleteMapping" "DELETE")))

