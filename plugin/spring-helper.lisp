(defpackage #:inga/plugin/spring-helper
  (:use #:cl)
  (:import-from #:alexandria
                #:switch)
  (:export #:convert-to-http-method))
(in-package #:inga/plugin/spring-helper)

(defun convert-to-http-method (type)
  (switch (type :test #'equal)
    ("GetMapping" "GET")
    ("PostMapping" "POST")  
    ("PutMapping" "PUT")
    ("DeleteMapping" "DELETE")))

