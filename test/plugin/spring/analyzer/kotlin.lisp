(defpackage #:inga/test/plugin/spring/analyzer/kotlin
  (:use #:cl
        #:fiveam
        #:inga/ast-index
        #:inga/plugin/spring/analyzer
        #:inga/test/helper
        #:inga/analyzer))
(in-package #:inga/test/plugin/spring/analyzer/kotlin)

(def-suite kotlin)
(in-suite kotlin)

(defparameter *spring-path* (merge-pathnames "test/plugin/spring/fixtures/general/"))

(test find-definitions-for-rest-server
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          `(((:type . :rest-server)
             (:host . "8080")
             (:name . "GET")
             (:path . "/kotlin/{string}")
             (:origin)
             (:file-pos .
              ((:type . :module-default)
               (:path . "src/main/kotlin/ingakt/server/RestControllerDefinition.kt")
               (:name . "get")
               (:fq-name . "ingakt.server.RestControllerDefinition.get-java.lang.String")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "src/main/kotlin/ingakt/server/RestControllerDefinition.kt" *spring-path*)
                        '((:line . 11) (:offset . 5))))))))
          (find-definitions
            (create-range "src/main/kotlin/ingakt/server/RestControllerDefinition.kt" :line 12))))))

(test find-references-for-rest-client
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          `(((:path . "src/main/kotlin/ingakt/client/ClientRestTemplate.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/ingakt/client/ClientRestTemplate.kt" *spring-path*)
                      '((:line . 11) (:offset . 29)))))
            ((:path . "src/main/kotlin/ingakt/client/ClientRestTemplate.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/ingakt/client/ClientRestTemplate.kt" *spring-path*)
                      '((:line . 27) (:offset . 29))))))
          (find-references
            `((:type . :rest-server)
              (:host . "8080")
              (:path . "/kotlin/path")
              (:name . "GET")
              (:file-pos .
               ((:path . "src/main/kotlin/ingakt/server/RestControllerDefinition.kt"))))
            *index*)))))

(test find-references-with-literal-for-rest-client
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          `(((:path . "src/main/kotlin/ingakt/client/StringLiteralHelper.kt")
             ;;              ↓
             ;; restTemplate.exchange("${this.path}", HttpMethod.GET, null, String::class.java)
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/ingakt/client/StringLiteralHelper.kt" *spring-path*)
                      '((:line . 13) (:offset . 22))))))
          (find-references
            `((:type . :rest-server)
              (:path . "/kotlin/string-literal-reference")
              (:name . "GET")
              (:file-pos .
               ((:path . "src/main/kotlin/ingakt/server/RestControllerDefinition.kt"))))
            *index*)))))

(test find-server-for-rest-template-with-uri-string
  (with-fixture jvm-ctx (*spring-path*)
    (let ((path "src/main/kotlin/ingakt/client/ClientRestTemplate.kt"))
      (is (equal
            `(((:host . "8080")
               (:method . "GET")
               (:path . "/kotlin/path")))
            (inga/plugin/spring/analyzer/kotlin::find-servers
              ;;                     ↓
              ;; return restTemplate.getForObject("http://localhost:8080/kotlin/path", String::class.java)
              (find-ast-in-ctx `((:path . ,path) (:line . 11) (:offset . 29)))
              path))))))

(test find-server-for-rest-template-with-uri-class
  (with-fixture jvm-ctx (*spring-path*)
    (let ((path "src/main/kotlin/ingakt/client/ClientRestTemplate.kt"))
      (is (equal
            `(((:host . "8080")
               (:method . "GET")
               (:path . "/kotlin/rest-template")))
            (inga/plugin/spring/analyzer/kotlin::find-servers
              ;;                     ↓
              ;; return restTemplate.getForObject(uri, String::class.java)
              (find-ast-in-ctx `((:path . ,path) (:line . 23) (:offset . 29)))
              path))))))

(test find-server-for-web-client-with-uri-string
  (with-fixture jvm-ctx (*spring-path*)
    (let ((path "src/main/kotlin/ingakt/client/ClientWebClient.kt"))
      (is (equal
            `(((:host . "8080")
               (:method . "GET")
               (:path . "/web-client")))
            (inga/plugin/spring/analyzer/kotlin::find-servers
              ;; .uri("/web-client")
              ;;  ↓
              ;; .retrieve()
              (find-ast-in-ctx `((:path . ,path) (:line . 12) (:offset . 14)))
              path))))))

(test find-server-for-web-client-with-uri-class
  (with-fixture jvm-ctx (*spring-path*)
    (let ((path "src/main/kotlin/ingakt/client/ClientWebClient.kt"))
      (is (equal
            `(((:host . "8080")
               (:method . "GET")
               (:path . "/web-client")))
            (inga/plugin/spring/analyzer/kotlin::find-servers
              ;; .uri(uri)
              ;;  ↓
              ;; .retrieve()
              (find-ast-in-ctx `((:path . ,path) (:line . 24) (:offset . 14)))
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
            :kotlin
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/kotlin/ingakt/server/RequestMappingDefinition.kt")
                         (:line . 8) (:offset . 1)))
                     '("ANNOTATION_ENTRY"))))))))

(test get-value-from-request-mapping-with-no-value
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("")
          ;; ↓
          ;; @RequestMapping
          (get-values-from-request-mapping
            :kotlin
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/kotlin/ingakt/server/RequestMappingDefinition.kt")
                         (:line . 11) (:offset . 5)))
                     '("ANNOTATION_ENTRY"))))))))

(test get-value-from-request-mapping-with-value
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @RequestMapping(value = "/{v}", method = RequestMethod.GET)
          (get-values-from-request-mapping
            :kotlin
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/kotlin/ingakt/server/RequestMappingDefinition.kt")
                         (:line . 15) (:offset . 5)))
                     '("ANNOTATION_ENTRY"))))))))

(test get-values-from-request-mapping-with-value
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("/{v1}" "/{v1}/{v2}")
          ;; ↓
          ;; @RequestMapping(value = ["/{v1}", "/{v1}/{v2}"], method = RequestMethod.GET)
          (get-values-from-request-mapping
            :kotlin
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/kotlin/ingakt/server/RequestMappingDefinition.kt")
                         (:line . 19) (:offset . 5)))
                     '("ANNOTATION_ENTRY"))))))))

(test get-value-from-request-mapping-with-path
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @RequestMapping(path = "/{v}", method = RequestMethod.GET)
          (get-values-from-request-mapping
            :kotlin
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/kotlin/ingakt/server/RequestMappingDefinition.kt")
                         (:line . 23) (:offset . 5)))
                     '("ANNOTATION_ENTRY"))))))))

(test get-values-from-request-mapping-with-path
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("/{v1}" "/{v1}/{v2}")
          ;; ↓
          ;; @RequestMapping(path = ["/{v1}", "/{v1}/{v2}"], method = RequestMethod.GET)
          (get-values-from-request-mapping
            :kotlin
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/kotlin/ingakt/server/RequestMappingDefinition.kt")
                         (:line . 27) (:offset . 5)))
                     '("ANNOTATION_ENTRY"))))))))

(test get-method-from-request-mapping
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "GET"
          ;; ↓
          ;; @RequestMapping(value = "/{v}", method = RequestMethod.GET)
          (get-method-from-request-mapping
            :kotlin
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/kotlin/ingakt/server/RequestMappingDefinition.kt")
                         (:line . 15) (:offset . 5)))
                     '("ANNOTATION_ENTRY"))))))))

;; GetMapping
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/GetMapping.html

(test get-value-from-get-mapping-with-single-member-annotation
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @GetMapping("/{v}")
          (get-values-from-request-mapping
            :kotlin
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/kotlin/ingakt/server/GetMappingDefinition.kt")
                         (:line . 14) (:offset . 5)))
                     '("ANNOTATION_ENTRY"))))))))

(test get-method-from-get-mapping
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "GET"
          ;; ↓
          ;; @GetMapping
          (get-method-from-request-mapping
            :kotlin
            (first (get-asts
                     (find-ast-in-ctx
                       '((:path . "src/main/kotlin/ingakt/server/GetMappingDefinition.kt")
                         (:line . 10) (:offset . 5)))
                     '("ANNOTATION_ENTRY"))))))))

;; PathVariable
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/PathVariable.html

(test find-param-from-path-variable-with-no-value
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "v"
          ;;           ↓
          ;; fun method(@PathVariable v: String) {
          (ast-value
            (find-param-from-path-variable
              :kotlin
              (find-ast-in-ctx
                '((:path . "src/main/kotlin/ingakt/server/PathVariableDefinition.kt")
                  (:line . 12) (:offset . 23)))
              "v")
            "name")))))

(test find-param-from-path-variable-with-single-member-annotation
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "a"
          ;;           ↓
          ;; fun method(@PathVariable("v") a: String) {
          (ast-value
            (find-param-from-path-variable
              :kotlin
              (find-ast-in-ctx
                '((:path . "src/main/kotlin/ingakt/server/PathVariableDefinition.kt")
                  (:line . 16) (:offset . 28)))
              "v")
            "name")))))

(test find-param-from-path-variable-with-value
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "a"
          ;;           ↓
          ;; fun method(@PathVariable(value = "v") a: String) {
          (ast-value
            (find-param-from-path-variable
              :kotlin
              (find-ast-in-ctx
                '((:path . "src/main/kotlin/ingakt/server/PathVariableDefinition.kt")
                  (:line . 20) (:offset . 21)))
              "v")
            "name")))))

(test find-param-from-path-variable-with-name
  (with-fixture jvm-ctx (*spring-path*)
    (is (equal
          "a"
          ;;           ↓
          ;; fun method(@PathVariable(name = "v") a: String) {
          (ast-value
            (find-param-from-path-variable
              :kotlin
              (find-ast-in-ctx
                '((:path . "src/main/kotlin/ingakt/server/PathVariableDefinition.kt")
                  (:line . 24) (:offset . 20)))
              "v")
            "name")))))

