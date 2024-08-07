(defpackage #:inga/plugin/spring/traversal/base
  (:use #:cl)
  (:import-from #:alexandria
                #:switch)
  (:import-from #:inga/traversal/base
                #:*rest-client-apis*
                #:start-traversal)
  (:export #:*rest-client-method-apis*
           #:*rest-client-path-apis*
           #:get-values-from-request-mapping
           #:get-method-from-request-mapping
           #:find-param-from-path-variable
           #:to-http-method))
(in-package #:inga/plugin/spring/traversal/base)

(defparameter *rest-client-method-apis* (make-hash-table))
(defparameter *rest-client-path-apis* (make-hash-table))

(defmethod start-traversal :after (kind include exclude path index)
  ;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
  (unless (gethash :rest-template *rest-client-apis*)
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-org.springframework.http.HttpEntity-org.springframework.core.ParameterizedTypeReference-java.lang.Object")
            (:path-i . 0)
            (:method-i . 1))
          (gethash :rest-template *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-org.springframework.http.HttpEntity-java.lang.Class")
            (:path-i . 0)
            (:method-i . 1))
          (gethash :rest-template *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-java.lang.Class")
            (:path-i . 0)
            (:method . "GET"))
          (gethash :rest-template *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-java.lang.Class-java.util.Map")
            (:path-i . 0)
            (:method . "GET"))
          (gethash :rest-template *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.getForObject-java.net.URI-java.lang.Class")
            (:path-i . 0)
            (:method . "GET"))
          (gethash :rest-template *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.postForObject-java.lang.String-java.lang.Object-java.lang.Class")
            (:path-i . 0)
            (:method . "POST"))
          (gethash :rest-template *rest-client-apis*)))

  ;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/reactive/function/client/WebClient.html
  (unless (gethash :web-client *rest-client-apis*)
    (push '((:type . :web-client)
            (:fq-name . "org.springframework.web.reactive.function.client.WebClient$RequestHeadersSpec.retrieve"))
          (gethash :web-client *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.reactive.function.client.WebClient$RequestHeadersUriSpec.uri-java.lang.String")
            (:path-i . 0))
          (gethash :web-client *rest-client-path-apis*))
    (push '((:fq-name . "org.springframework.web.reactive.function.client.WebClient.get")
            (:method . "GET"))
          (gethash :web-client *rest-client-method-apis*))
    (push '((:fq-name . "org.springframework.web.reactive.function.client.WebClient.post")
            (:method . "POST"))
          (gethash :web-client *rest-client-method-apis*))))

(defgeneric get-values-from-request-mapping (type ast))

(defgeneric get-method-from-request-mapping (type ast))

(defgeneric find-param-from-path-variable (type ast target-name))

(defun to-http-method (type)
  (switch (type :test #'equal)
    ("GetMapping" "GET")
    ("PostMapping" "POST")  
    ("PutMapping" "PUT")
    ("DeleteMapping" "DELETE")))

