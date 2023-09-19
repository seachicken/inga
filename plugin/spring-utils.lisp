(defpackage #:inga/plugin/spring-utils
  (:use #:cl)
  (:export #:convert-to-http-method))
(in-package #:inga/plugin/spring-utils)

(defun convert-to-http-method (type)
  (alexandria:switch (type :test #'equal)
    ("GetMapping" "GET")
    ("PostMapping" "POST")  
    ("PutMapping" "PUT")
    ("DeleteMapping" "DELETE")))

