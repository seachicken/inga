(defpackage #:inga/test/traversal/spring-java
  (:use #:cl
        #:fiveam
        #:inga/traversal
        #:inga/ast-index
        #:inga/test/helper))
(in-package #:inga/test/traversal/spring-java)

(def-suite java)
(in-suite java)

(defparameter *java-path* (merge-pathnames "test/fixtures/java/"))

;; RequestMapping
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/RequestMapping.html

(test get-value-from-request-mapping-with-single-member-annotation
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "/mapping"
          ;; ↓
          ;; @RequestMapping("/")
          (get-value-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.java"
                               '((:line . 8) (:offset . 1)))
                     '("ANNOTATION"))))))))

(test get-value-from-request-mapping-with-no-value
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          ""
          ;; ↓
          ;; @RequestMapping
          (get-value-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.java"
                               '((:line . 11) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-value-from-request-mapping-with-value
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "/{v}"
          ;; ↓
          ;; @RequestMapping(value = "/{v}", method = RequestMethod.GET)
          (get-value-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.java"
                               '((:line . 15) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-value-from-request-mapping-with-path
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "/{v}"
          ;; ↓
          ;; @RequestMapping(path = "/{v}", method = RequestMethod.GET)
          (get-value-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.java"
                               '((:line . 19) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-method-from-request-mapping
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "GET"
          ;; ↓
          ;; @RequestMapping(value = "/{v}", method = RequestMethod.GET)
          (get-method-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.java"
                               '((:line . 15) (:offset . 5)))
                     '("ANNOTATION"))))))))

;; PathVariable
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/PathVariable.html

(test get-value-from-path-variable-with-no-value
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "v"
          ;;                    ↓
          ;; public void method(@PathVariable String v) {
          (get-value-from-path-variable
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.java"
                               '((:line . 11) (:offset . 32)))
                     '("ANNOTATION"))))))))

(test get-value-from-path-variable-with-single-member-annotation
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "v"
          ;;                    ↓
          ;; public void method(@PathVariable("v") String v) {
          (get-value-from-path-variable
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.java"
                               '((:line . 15) (:offset . 37)))
                     '("ANNOTATION"))))))))

(test get-value-from-path-variable-with-value
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "v"
          ;;                    ↓
          ;; public void method(@PathVariable(value = "v") String v) {
          (get-value-from-path-variable
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.java"
                               '((:line . 19) (:offset . 30)))
                     '("ANNOTATION"))))))))

(test get-value-from-path-variable-with-name
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "v"
          ;;                    ↓
          ;; public void method(@PathVariable(name = "v") String v) {
          (get-value-from-path-variable
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.java"
                               '((:line . 23) (:offset . 29)))
                     '("ANNOTATION"))))))))
