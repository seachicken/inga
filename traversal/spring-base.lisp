(defpackage #:inga/traversal/spring-base
  (:use #:cl)
  (:import-from #:alexandria
                #:switch)
  (:export #:get-values-from-request-mapping
           #:get-method-from-request-mapping
           #:find-param-from-path-variable
           #:to-http-method))
(in-package #:inga/traversal/spring-base)

(defgeneric get-values-from-request-mapping (type ast))

(defgeneric get-method-from-request-mapping (type ast))

(defgeneric find-param-from-path-variable (type ast target-name))

(defun to-http-method (type)
  (switch (type :test #'equal)
    ("GetMapping" "GET")
    ("PostMapping" "POST")  
    ("PutMapping" "PUT")
    ("DeleteMapping" "DELETE")))

