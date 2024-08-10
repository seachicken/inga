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

(defmethod start-traversal :after (kind include exclude path index)
  ;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
  (unless (gethash :spring *rest-client-apis*)
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-org.springframework.http.HttpEntity-org.springframework.core.ParameterizedTypeReference-java.lang.Object")
            (:call-type . :rest-template)
            (:path-i . 0)
            (:method-i . 1))
          (gethash :spring *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-org.springframework.http.HttpEntity-java.lang.Class")
            (:call-type . :rest-template)
            (:path-i . 0)
            (:method-i . 1))
          (gethash :spring *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-java.lang.Class")
            (:call-type . :rest-template)
            (:path-i . 0)
            (:method . "GET"))
          (gethash :spring *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-java.lang.Class-java.util.Map")
            (:call-type . :rest-template)
            (:path-i . 0)
            (:method . "GET"))
          (gethash :spring *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.getForObject-java.net.URI-java.lang.Class")
            (:call-type . :rest-template)
            (:method . "GET"))
          (gethash :spring *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.postForObject-java.lang.String-java.lang.Object-java.lang.Class")
            (:call-type . :rest-template)
            (:path-i . 0)
            (:method . "POST"))
          (gethash :spring *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-java.lang.Class")
            (:call-type . :rest-template)
            (:path-i . 0)
            (:method . "GET"))
          (gethash :spring *rest-client-apis*))

    ;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/reactive/function/client/WebClient.html
    (push '((:fq-name . "org.springframework.web.reactive.function.client.WebClient$RequestHeadersSpec.retrieve")
            (:call-type . :web-client))
          (gethash :spring *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.reactive.function.client.WebClient.create-java.lang.String")
            (:host-i . 0))
          (gethash :spring *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.reactive.function.client.WebClient.get")
            (:method . "GET"))
          (gethash :spring *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.reactive.function.client.WebClient.post")
            (:method . "POST"))
          (gethash :spring *rest-client-apis*))
    (push '((:fq-name . "org.springframework.web.reactive.function.client.WebClient$RequestHeadersUriSpec.uri-java.lang.String")
            (:path-i . 0))
          (gethash :spring *rest-client-apis*))

    ;; common APIs
    (push '((:fq-name . "org.springframework.web.util.UriComponentsBuilder.fromUriString-java.lang.String")
            (:host-i . 0))
          (gethash :spring *rest-client-apis*))  
    (push '((:fq-name . "org.springframework.web.util.UriComponentsBuilder.path-java.lang.String")
            (:path-i . 0))
          (gethash :spring *rest-client-apis*))))

(defgeneric get-values-from-request-mapping (type ast))

(defgeneric get-method-from-request-mapping (type ast))

(defgeneric find-param-from-path-variable (type ast target-name))

(defun to-http-method (type)
  (switch (type :test #'equal)
    ("GetMapping" "GET")
    ("PostMapping" "POST")  
    ("PutMapping" "PUT")
    ("DeleteMapping" "DELETE")))

