(defpackage #:inga/test/traversal/spring-kotlin
  (:use #:cl
        #:fiveam
        #:inga/traversal
        #:inga/ast-index
        #:inga/test/helper))
(in-package #:inga/test/traversal/spring-kotlin)

(def-suite kotlin)
(in-suite kotlin)

(defparameter *kotlin-path* (merge-pathnames "test/fixtures/kotlin/"))

(test find-definitions-for-rest-server-get-method
  (with-fixture jvm-context (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:type . :rest-server)
             (:host . "8080")
             (:name . "GET")
             (:path . "/{string}")
             (:origin)
             (:file-pos .
              (;; TODO: add scope
               ;;(:type . :module-public)
               (:path . "p1/server/spring/src/main/p1/RestControllerDefinition.kt")
               (:name . "get")
               (:fq-name . "p1.RestControllerDefinition.get-java.lang.String")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "p1/server/spring/src/main/p1/RestControllerDefinition.kt" *kotlin-path*)
                        '((:line . 14) (:offset . 5))))))))
          (find-definitions
            (create-range "p1/server/spring/src/main/p1/RestControllerDefinition.kt" :line 15))))))

(test find-references-for-rest-client-get-method
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          `(((:path . "p1/client/ClientRestTemplate.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/client/ClientRestTemplate.kt" *kotlin-path*)
                      '((:line . 10) (:offset . 29)))))
            ((:path . "p1/client/ClientRestTemplate.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/client/ClientRestTemplate.kt" *kotlin-path*)
                      '((:line . 18) (:offset . 29))))))
          (find-references
            `((:type . :rest-server)
              (:host . "8080")
              (:path . "/path")
              (:name . "GET")
              (:file-pos .
               ((:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java"))))
            *index*)))))

(test find-references-for-rest-client-post-method
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          `(((:path . "p1/client/ClientRestTemplate.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/client/ClientRestTemplate.kt" *kotlin-path*)
                      '((:line . 22) (:offset . 29))))))
          (find-references
            `((:type . :rest-server)
              (:host . "8080")
              (:path . "/path")
              (:name . "POST")
              (:file-pos .
               ((:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java"))))
            *index*)))))

;; RequestMapping
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/RequestMapping.html

(test get-value-from-request-mapping-with-single-member-annotation
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          '("/mapping")
          ;; ↓
          ;; @RequestMapping("/")
          (get-values-from-request-mapping
            :kotlin
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.kt"
                               '((:line . 8) (:offset . 1)) :key-offset "textOffset")
                     '("ANNOTATION_ENTRY"))))))))

(test get-value-from-request-mapping-with-no-value
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          '("")
          ;; ↓
          ;; @RequestMapping
          (get-values-from-request-mapping
            :kotlin
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.kt"
                               '((:line . 11) (:offset . 5)) :key-offset "textOffset")
                     '("ANNOTATION_ENTRY"))))))))

(test get-value-from-request-mapping-with-value
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @RequestMapping(value = "/{v}", method = RequestMethod.GET)
          (get-values-from-request-mapping
            :kotlin
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.kt"
                               '((:line . 15) (:offset . 5)) :key-offset "textOffset")
                     '("ANNOTATION_ENTRY"))))))))

(test get-values-from-request-mapping-with-value
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          '("/{v1}" "/{v1}/{v2}")
          ;; ↓
          ;; @RequestMapping(value = ["/{v1}", "/{v1}/{v2}"], method = RequestMethod.GET)
          (get-values-from-request-mapping
            :kotlin
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.kt"
                               '((:line . 19) (:offset . 5)) :key-offset "textOffset")
                     '("ANNOTATION_ENTRY"))))))))

(test get-value-from-request-mapping-with-path
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @RequestMapping(path = "/{v}", method = RequestMethod.GET)
          (get-values-from-request-mapping
            :kotlin
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.kt"
                               '((:line . 23) (:offset . 5)) :key-offset "textOffset")
                     '("ANNOTATION_ENTRY"))))))))

(test get-values-from-request-mapping-with-path
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          '("/{v1}" "/{v1}/{v2}")
          ;; ↓
          ;; @RequestMapping(path = ["/{v1}", "/{v1}/{v2}"], method = RequestMethod.GET)
          (get-values-from-request-mapping
            :kotlin
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.kt"
                               '((:line . 27) (:offset . 5)) :key-offset "textOffset")
                     '("ANNOTATION_ENTRY"))))))))

(test get-method-from-request-mapping
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          "GET"
          ;; ↓
          ;; @RequestMapping(value = "/{v}", method = RequestMethod.GET)
          (get-method-from-request-mapping
            :kotlin
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.kt"
                               '((:line . 15) (:offset . 5)) :key-offset "textOffset")
                     '("ANNOTATION_ENTRY"))))))))

;; GetMapping
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/GetMapping.html

(test get-value-from-get-mapping-with-single-member-annotation
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @GetMapping("/{v}")
          (get-values-from-request-mapping
            :kotlin
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/GetMappingDefinition.kt"
                               '((:line . 13) (:offset . 5)) :key-offset "textOffset")
                     '("ANNOTATION_ENTRY"))))))))

(test get-method-from-get-mapping
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          "GET"
          ;; ↓
          ;; @GetMapping
          (get-method-from-request-mapping
            :kotlin
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/GetMappingDefinition.kt"
                               '((:line . 9) (:offset . 5)) :key-offset "textOffset")
                     '("ANNOTATION_ENTRY"))))))))

;; PathVariable
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/PathVariable.html

(test find-param-from-path-variable-with-no-value
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          "v"
          ;;           ↓
          ;; fun method(@PathVariable v: String) {
          (trav:ast-value
            (find-param-from-path-variable
              :kotlin
              (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.kt"
                        '((:line . 11) (:offset . 23)) :key-offset "textOffset")
              "v")
            "name")))))

(test find-param-from-path-variable-with-single-member-annotation
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          "a"
          ;;           ↓
          ;; fun method(@PathVariable("v") a: String) {
          (trav:ast-value
            (find-param-from-path-variable
              :kotlin
              (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.kt"
                        '((:line . 15) (:offset . 28)) :key-offset "textOffset")
              "v")
            "name")))))

(test find-param-from-path-variable-with-value
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          "a"
          ;;           ↓
          ;; fun method(@PathVariable(value = "v") a: String) {
          (trav:ast-value
            (find-param-from-path-variable
              :kotlin
              (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.kt"
                        '((:line . 19) (:offset . 21)) :key-offset "textOffset")
              "v")
            "name")))))

(test find-param-from-path-variable-with-name
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          "a"
          ;;           ↓
          ;; fun method(@PathVariable(name = "v") a: String) {
          (trav:ast-value
            (find-param-from-path-variable
              :kotlin
              (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.kt"
                        '((:line . 23) (:offset . 20)) :key-offset "textOffset")
              "v")
            "name")))))

