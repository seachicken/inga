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

(test find-definitions-for-spring-rest-get-method
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
    (is (equal
          `(((:type . :rest-server)
             (:host . "8080")
             (:name . "GET")
             (:path . "/{string}")
             (:file-pos .
              ((:type . :module-public)
               (:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
               (:name . "get")
               (:fq-name . "p1.RestControllerDefinition.get-java.lang.String")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                        '((:line . 20) (:offset . 17))))))))
          (find-definitions
            (create-range "p1/server/spring/src/main/p1/RestControllerDefinition.java" :line 20))))))

;; RequestMapping
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/RequestMapping.html

(test get-value-from-request-mapping-with-single-member-annotation
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          '("/mapping")
          ;; ↓
          ;; @RequestMapping("/")
          (get-values-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.java"
                               '((:line . 8) (:offset . 1)))
                     '("ANNOTATION"))))))))

(test get-value-from-request-mapping-with-no-value
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          '("")
          ;; ↓
          ;; @RequestMapping
          (get-values-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.java"
                               '((:line . 11) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-value-from-request-mapping-with-value
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @RequestMapping(value = "/{v}", method = RequestMethod.GET)
          (get-values-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.java"
                               '((:line . 15) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-values-from-request-mapping-with-value
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          '("/{v1}" "/{v1}/{v2}")
          ;; ↓
          ;; @RequestMapping(value = {"/{v1}", "/{v1}/{v2}"}, method = RequestMethod.GET)
          (get-values-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.java"
                               '((:line . 19) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-value-from-request-mapping-with-path
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @RequestMapping(path = "/{v}", method = RequestMethod.GET)
          (get-values-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.java"
                               '((:line . 23) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-values-from-request-mapping-with-path
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          '("/{v1}" "/{v1}/{v2}")
          ;; ↓
          ;; @RequestMapping(path = {"/{v1}", "/{v1}/{v2}"}, method = RequestMethod.GET)
          (get-values-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/RequestMappingDefinition.java"
                               '((:line . 27) (:offset . 5)))
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

;; GetMapping
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/GetMapping.html

(test get-value-from-get-mapping-with-single-member-annotation
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          '("/{v}")
          ;; ↓
          ;; @GetMapping("/{v}")
          (get-values-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/GetMappingDefinition.java"
                               '((:line . 13) (:offset . 5)))
                     '("ANNOTATION"))))))))

(test get-method-from-get-mapping
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "GET"
          ;; ↓
          ;; @GetMapping
          (get-method-from-request-mapping
            :java
            (first (trav:get-asts
                     (find-ast "p1/server/spring/src/main/p1/GetMappingDefinition.java"
                               '((:line . 9) (:offset . 5)))
                     '("ANNOTATION"))))))))

;; PathVariable
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/bind/annotation/PathVariable.html

(test find-param-from-path-variable-with-no-value
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "v"
          ;;             ↓
          ;; public void method(@PathVariable String v) {
          (trav:ast-value
            (find-param-from-path-variable
              :java
              (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.java"
                        '((:line . 11) (:offset . 17)))
              "v")
            "name")))))

(test find-param-from-path-variable-with-single-member-annotation
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "a"
          ;;             ↓
          ;; public void method(@PathVariable("v") String v) {
          (trav:ast-value
            (find-param-from-path-variable
              :java
              (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.java"
                        '((:line . 15) (:offset . 17)))
              "v")
            "name")))))

(test find-param-from-path-variable-with-value
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "a"
          ;;             ↓
          ;; public void method(@PathVariable(value = "v") String v) {
          (trav:ast-value
            (find-param-from-path-variable
              :java
              (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.java"
                        '((:line . 19) (:offset . 17)))
              "v")
            "name")))))

(test find-param-from-path-variable-with-name
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (is (equal
          "a"
          ;;                   ↓
          ;; public void method(@PathVariable(name = "v") String v) {
          (trav:ast-value
            (find-param-from-path-variable
              :java
              (find-ast "p1/server/spring/src/main/p1/PathVariableDefinition.java"
                        '((:line . 23) (:offset . 17)))
              "v")
            "name")))))

