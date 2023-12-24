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

