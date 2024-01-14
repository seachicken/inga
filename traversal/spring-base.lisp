(defpackage #:inga/traversal/spring-base
  (:use #:cl)
  (:import-from #:alexandria
                #:switch)
  (:import-from #:inga/traversal/base
                #:*rest-client-apis*
                #:start-traversal)
  (:export #:get-values-from-request-mapping
           #:get-method-from-request-mapping
           #:find-param-from-path-variable
           #:to-http-method))
(in-package #:inga/traversal/spring-base)

(defmethod start-traversal :after (kind include exclude path index)
  ;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
  (when (gethash :rest-template *rest-client-apis*)
    (return-from start-traversal))

  (push '((:fq-name . "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-org.springframework.http.HttpEntity-org.springframework.core.ParameterizedTypeReference-java.lang.Object")
          (:path-i . 0)
          (:method-i . 1))
        (gethash :rest-template *rest-client-apis*))
  (push '((:fq-name . "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-org.springframework.http.HttpEntity-java.lang.Class")
          (:path-i . 0)
          (:method-i . 1))
        (gethash :rest-template *rest-client-apis*)))

(defgeneric get-values-from-request-mapping (type ast))

(defgeneric get-method-from-request-mapping (type ast))

(defgeneric find-param-from-path-variable (type ast target-name))

(defun to-http-method (type)
  (switch (type :test #'equal)
    ("GetMapping" "GET")
    ("PostMapping" "POST")  
    ("PutMapping" "PUT")
    ("DeleteMapping" "DELETE")))

