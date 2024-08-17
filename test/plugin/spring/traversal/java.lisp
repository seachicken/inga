(defpackage #:inga/test/plugin/spring/traversal/java
  (:use #:cl
        #:fiveam
        #:inga/ast-index
        #:inga/plugin/spring/traversal
        #:inga/test/helper  
        #:inga/traversal))
(in-package #:inga/test/plugin/spring/traversal/java)

(def-suite java)
(in-suite java)

(defparameter *spring-path* (merge-pathnames "test/plugin/spring/fixtures/general/"))

(test find-definitions-for-rest-server
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          `(((:type . :rest-server)
             (:host . "8080")
             (:name . "GET")
             (:path . "/{string}")
             (:origin)
             (:file-pos .
              ((:type . :module-public)
               (:path . "src/main/java/inga/server/RestControllerDefinition.java")
               (:name . "get")
               (:fq-name . "inga.server.RestControllerDefinition.get-java.lang.String")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "src/main/java/inga/server/RestControllerDefinition.java" *spring-path*)
                        '((:line . 12) (:offset . 17))))))))
          (find-definitions
            (create-range "src/main/java/inga/server/RestControllerDefinition.java" :line 12))))))

(test find-references-for-rest-client
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          `(((:path . "src/main/java/inga/client/ClientRestTemplate.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/inga/client/ClientRestTemplate.java" *spring-path*)
                      '((:line . 17) (:offset . 41)))))
            ((:path . "src/main/java/inga/client/ClientRestTemplate.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/inga/client/ClientRestTemplate.java" *spring-path*)
                      '((:line . 33) (:offset . 37))))))
          (find-references
            `((:type . :rest-server)
              (:host . "8080")
              (:path . "/path")
              (:name . "GET")
              (:file-pos .
               ((:path . "src/main/java/inga/server/RestControllerDefinition.java"))))
            *index*)))))

(test find-references-with-literal-for-rest-client
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          `(((:path . "src/main/java/inga/client/StringLiteralHelper.java")
             ;;                      ↓
             ;; restTemplate.exchange(path, HttpMethod.GET, null, String.class);
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/inga/client/StringLiteralHelper.java"
                                       *spring-path*)
                      '((:line . 16) (:offset . 34))))))
          (find-references
            `((:type . :rest-server)
              (:path . "/string-literal-reference")
              (:name . "GET")
              (:file-pos .
               ((:path . "src/main/java/inga/server/RestControllerDefinition.java"))))
            *index*)))))

(test find-server-for-rest-template-with-uri-string
  (with-fixture jvm-ctx (*spring-path*)
    (let ((path "src/main/java/inga/client/ClientRestTemplate.java"))
      (is (equal
            `(((:host . "8080")
               (:method . "GET")
               (:path . "/path")))
            (inga/plugin/spring/traversal/java::find-servers
              ;;                                 ↓
              ;; return restTemplate.getForObject("http://localhost:8080/path", String.class);
              (find-ast-in-ctx `((:path . ,path) (:line . 17) (:offset . 41)))
              path))))))

(test find-server-for-rest-template-with-uri-class
  (with-fixture jvm-ctx (*spring-path*)
    (let ((path "src/main/java/inga/client/ClientRestTemplate.java"))
      (is (equal
            `(((:host . "8080")
               (:method . "GET")
               (:path . "/rest-template")))
            (inga/plugin/spring/traversal/java::find-servers
              ;;                                 ↓
              ;; return restTemplate.getForObject(uri, String.class);
              (find-ast-in-ctx `((:path . ,path) (:line . 29) (:offset . 41)))
              path))))))

(test find-server-for-web-client-with-uri-string
  (with-fixture jvm-ctx (*spring-path*)
    (let ((path "src/main/java/inga/client/ClientWebClient.java"))
      (is (equal
            `(((:host . "8080")
               (:method . "GET")
               (:path . "/web-client")))
            (inga/plugin/spring/traversal/java::find-servers
              ;; .uri("/web-client")
              ;;          ↓
              ;; .retrieve()
              (find-ast-in-ctx `((:path . ,path) (:line . 12) (:offset . 26)))
              path))))))

(test find-server-for-web-client-with-uri-class
  (with-fixture jvm-ctx (*spring-path*)
    (let ((path "src/main/java/inga/client/ClientWebClient.java"))
      (is (equal
            `(((:host . "8080")
               (:method . "GET")
               (:path . "/web-client")))
            (inga/plugin/spring/traversal/java::find-servers
              ;; .uri(uri)
              ;;          ↓
              ;; .retrieve()
              (find-ast-in-ctx `((:path . ,path) (:line . 24) (:offset . 26)))
              path))))))

;; RequestMapping
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/RequestMapping.html

(test get-value-from-request-mapping-with-single-member-annotation
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("/mapping")
          ;; ↓
          ;; @RequestMapping("/")
          (get-values-from-request-mapping
            :java
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/java/inga/server/RequestMappingDefinition.java")
                         (:line . 8) (:offset . 1)))
                     '("ANNOTATION"))))))))

(test get-value-from-request-mapping-with-no-value
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("")
          ;; ↓
          ;; @RequestMapping
          (get-values-from-request-mapping
            :java
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/java/inga/server/RequestMappingDefinition.java")
                         (:line . 11) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-value-from-request-mapping-with-value
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @RequestMapping(value = "/{v}", method = RequestMethod.GET)
          (get-values-from-request-mapping
            :java
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/java/inga/server/RequestMappingDefinition.java")
                         (:line . 15) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-values-from-request-mapping-with-value
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("/{v1}" "/{v1}/{v2}")
          ;; ↓
          ;; @RequestMapping(value = {"/{v1}", "/{v1}/{v2}"}, method = RequestMethod.GET)
          (get-values-from-request-mapping
            :java
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/java/inga/server/RequestMappingDefinition.java")
                         (:line . 19) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-value-from-request-mapping-with-path
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @RequestMapping(path = "/{v}", method = RequestMethod.GET)
          (get-values-from-request-mapping
            :java
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/java/inga/server/RequestMappingDefinition.java")
                         (:line . 23) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-values-from-request-mapping-with-path
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("/{v1}" "/{v1}/{v2}")
          ;; ↓
          ;; @RequestMapping(path = {"/{v1}", "/{v1}/{v2}"}, method = RequestMethod.GET)
          (get-values-from-request-mapping
            :java
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/java/inga/server/RequestMappingDefinition.java")
                         (:line . 27) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-method-from-request-mapping
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "GET"
          ;; ↓
          ;; @RequestMapping(value = "/{v}", method = RequestMethod.GET)
          (get-method-from-request-mapping
            :java
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/java/inga/server/RequestMappingDefinition.java")
                         (:line . 15) (:offset . 5)))
                     '("ANNOTATION"))))))))

;; GetMapping
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/GetMapping.html

(test get-value-from-get-mapping-with-single-member-annotation
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @GetMapping("/{v}")
          (get-values-from-request-mapping
            :java
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/java/inga/server/GetMappingDefinition.java")
                         (:line . 14) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-method-from-get-mapping
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "GET"
          ;; ↓
          ;; @GetMapping
          (get-method-from-request-mapping
            :java
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/java/inga/server/GetMappingDefinition.java")
                         (:line . 10) (:offset . 5)))
                     '("ANNOTATION"))))))))

;; PathVariable
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/PathVariable.html

(test find-param-from-path-variable-with-no-value
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "v"
          ;;             ↓
          ;; public void method(@PathVariable String v) {
          (ast-value
            (find-param-from-path-variable
              :java
              (find-ast-in-ctx
                '((:path . "src/main/java/inga/server/PathVariableDefinition.java")
                  (:line . 12) (:offset . 17)))
              "v")
            "name")))))

(test find-param-from-path-variable-with-single-member-annotation
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "a"
          ;;             ↓
          ;; public void method(@PathVariable("v") String v) {
          (ast-value
            (find-param-from-path-variable
              :java
              (find-ast-in-ctx
                '((:path . "src/main/java/inga/server/PathVariableDefinition.java")
                  (:line . 16) (:offset . 17)))
              "v")
            "name")))))

(test find-param-from-path-variable-with-value
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "a"
          ;;             ↓
          ;; public void method(@PathVariable(value = "v") String v) {
          (ast-value
            (find-param-from-path-variable
              :java
              (find-ast-in-ctx
                '((:path . "src/main/java/inga/server/PathVariableDefinition.java")
                  (:line . 20) (:offset . 17)))
              "v")
            "name")))))

(test find-param-from-path-variable-with-name
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "a"
          ;;                   ↓
          ;; public void method(@PathVariable(name = "v") String v) {
          (ast-value
            (find-param-from-path-variable
              :java
              (find-ast-in-ctx
                '((:path . "src/main/java/inga/server/PathVariableDefinition.java")
                  (:line . 24) (:offset . 17)))
              "v")
            "name")))))

