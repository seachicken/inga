(defpackage #:inga/test/traversal/spring-java
  (:use #:cl
        #:fiveam
        #:inga/traversal
        #:inga/ast-index
        #:inga/test/helper))
(in-package #:inga/test/traversal/spring-java)

(def-suite java)
(in-suite java)

(defparameter *java-path* (merge-pathnames #p"test/fixtures/java/"))

;; PathVariable
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/PathVariable.html

(test get-value-from-path-variable-with-single-member-annotation
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "v"
          ;;                    ↓
          ;; public void method(@PathVariable("v") String v) {
          (get-value-from-path-variable
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RestControllerDefinition.java"
                               '((:line . 16) (:offset . 21)))
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
                     (find-ast "p1/server/spring/src/main/p1/RestControllerDefinition.java"
                               '((:line . 43) (:offset . 30)))
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
                     (find-ast "p1/server/spring/src/main/p1/RestControllerDefinition.java"
                               '((:line . 47) (:offset . 29)))
                     '("ANNOTATION"))))))))

(test get-value-from-path-variable-with-no-value
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "v"
          ;;                    ↓
          ;; public void method(@PathVariable String v) {
          (get-value-from-path-variable
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RestControllerDefinition.java"
                               '((:line . 51) (:offset . 32)))
                     '("ANNOTATION"))))))))

