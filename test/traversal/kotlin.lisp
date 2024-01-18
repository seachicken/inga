(defpackage #:inga/test/traversal/kotlin
  (:use #:cl
        #:fiveam
        #:inga/traversal
        #:inga/ast-index
        #:inga/test/helper))
(in-package #:inga/test/traversal/kotlin)

(def-suite kotlin)
(in-suite kotlin)

(defparameter *kotlin-path* (merge-pathnames "test/fixtures/kotlin/"))

(test find-definitions-for-method
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/PrimaryConstructorDefinition.kt")
             (:name . "method")
             (:fq-name . "p1.PrimaryConstructorDefinition.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/PrimaryConstructorDefinition.kt" *kotlin-path*)
                      '((:line . 4) (:offset . 5))))))
          (find-definitions (create-range "p1/PrimaryConstructorDefinition.kt" :line 4))))))

(test find-references-for-primary-constructor
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/PrimaryConstructorReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/PrimaryConstructorReference.kt" *kotlin-path*)
                      '((:line . 7) (:offset . 11))))))
          (find-references
            '((:path . "p1/PrimaryConstructorHelper.kt")
              (:name . "method")
              (:fq-name . "p1.PrimaryConstructorHelper.method"))
            *index*)))))

(test find-references-for-fq-method
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/FqMethodReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/FqMethodReference.kt" *kotlin-path*)
                      '((:line . 5) (:offset . 29))))))
          (find-references
            '((:path . "p1/FqMethodHelper.kt")
              (:name . "method")
              (:fq-name . "p1.FqMethodHelper.method"))
            *index*)))))

(test find-references-for-java-class
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/JavaReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/JavaReference.kt" *kotlin-path*)
                      '((:line . 7) (:offset . 11))))))
          (find-references
            '((:path . "p1/KotlinReference.java")
              (:name . "method")
              (:fq-name . "p1.KotlinReference.method"))
            *index*)))))

(test find-fq-name-for-reference-with-string-literal
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/client/ClientRestTemplate.kt"))
      (is (equal
            "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-java.lang.Class"
            (find-fq-name
              ;;                     ↓
              ;; return restTemplate.getForObject("http://localhost:8080/path", String::class.java)
              (find-ast-in-ctx `((:path . ,path) (:line . 10) (:offset . 29)))
              path))))))

(test find-fq-name-for-reference-with-enum
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/client/ClientRestTemplate.kt"))
      (is (equal
            "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-NULL-java.lang.Class"
            (find-fq-name
              ;;                     ↓
              ;; return restTemplate.exchange("http://localhost:8080/path", HttpMethod.GET, null, String::class.java)
              (find-ast-in-ctx `((:path . ,path) (:line . 18) (:offset . 29)))
              path))))))

(test find-fq-name-for-reference-with-properties
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/PropertyReference.kt"))
      (is (equal
            "p1.PropertyHelper.method-java.lang.String"
            (find-fq-name
              (find-ast-in-ctx `((:path . ,path) (:line . 9) (:offset . 11)))
              path))))))

(test find-fq-name-for-reference-with-new-class
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/client/ClientRestTemplate.kt"))
      (is (equal
            "org.springframework.web.client.RestTemplate.postForObject-java.lang.String-java.lang.String-java.lang.Class"
            (find-fq-name
              ;;                     ↓
              ;; return restTemplate.postForObject(
              (find-ast-in-ctx `((:path . ,path) (:line . 22) (:offset . 29)))
              path))))))

(test find-fq-name-for-reference-with-not-null-parameter
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/ParameterReference.kt"))
      (is (equal
            "p1.ParameterHelper.method"
            (find-fq-name
              ;; fun method(v: Helper) {
              ;;     ↓
              ;;   v.method()
              ;; }
              (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 11)))
              path))))))

(test find-fq-name-for-reference-with-nullable-parameter
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/ParameterReference.kt"))
      (is (equal
            "p1.ParameterHelper.method"
            (find-fq-name
              ;; fun method(v: Helper?) {
              ;;     ↓
              ;;   v.method()
              ;; }
              (find-ast-in-ctx `((:path . ,path) (:line . 11) (:offset . 11)))
              path))))))

(test find-fq-name-for-reference-with-vararg
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/VarargReference.kt"))
      (is (equal
            "p1.VarargReference.method-p1.VarargHelper"
            (find-fq-name
              ;; fun method(vararg vs: Helper) {
              ;;   ↓
              ;;   method(vs)
              ;; }
              (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 9)))
              path))))))

(test find-fq-name-for-reference-with-anonymous-objects
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/AnonymousObjectReference.kt"))
      (is (equal
            "java.lang.Thread.Thread-java.lang.Runnable"
            (find-fq-name
              ;; ↓
              ;; Thread(object : Runnable {
              (first (trav:get-asts (find-ast-in-ctx `((:path . ,path) (:line . 10) (:offset . 9)))
                                    '("CALL_EXPRESSION")))
              path))))))

(test find-fq-name-for-reference-with-anonymous-objects-super-call
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/AnonymousObjectReference.kt"))
      (is (equal
            "p1.AnonymousObjectHelper.method-org.springframework.core.ParameterizedTypeReference"
            (find-fq-name
              ;;   ↓
              ;; v.method(object : ParameterizedTypeReference<T>() {})
              (find-ast-in-ctx `((:path . ,path) (:line . 17) (:offset . 11)))
              path))))))

(test find-fq-name-for-method-chain-first
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/TypeInferenceReference.kt"))
      (is (equal
            "kotlin.collections.CollectionsKt.listOf-java.lang.String"
            (find-fq-name
              ;; ↓
              ;; listOf("a").forEach { println(it) }
              (first (trav:get-asts
                       (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 9)))
                       '("CALL_EXPRESSION")))
              path))))))

(test find-fq-name-for-method-chain-second
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/TypeInferenceReference.kt"))
      (is (equal
            "kotlin.collections.CollectionsKt.forEach-FUNCTION_LITERAL"
            (find-fq-name
              ;;             ↓
              ;; listOf("a").forEach { println(it) }
              (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 21)))
              path))))))

(test find-fq-class-name-for-type-inference-in-lambda
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/TypeInferenceReference.kt"))
      (is (equal
            "java.lang.String"
            ;;                               ↓
            ;; listOf("a").forEach { println(it) }
            (find-fq-class-name
              (first (trav:get-asts
                       (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 39)))
                       '("REFERENCE_EXPRESSION")))
              path))))))

(test find-fq-class-name-for-reference-type-inference-in-lambda
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/TypeInferenceReference.kt"))
      (is (equal
            "java.lang.String"
            (find-fq-class-name
              (first (trav:get-asts
                       (find-ast-in-ctx `((:path . ,path) (:line . 10) (:offset . 32)))
                       '("REFERENCE_EXPRESSION")))
              path))))))

(test get-dot-expressions-with-zero-dot
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/PrimaryConstructorDefinition.kt"))
      (is (equal
            '("p1")
            (inga/traversal/kotlin::get-dot-expressions
              ;;         ↓
              ;; package p1
              (find-ast-in-ctx `((:path . ,path) (:line . 1) (:offset . 9)))))))))

(test get-dot-expressions-with-one-dot
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/client/ClientRestTemplate.kt"))
      (is (equal
            '("p1" "client")
            (inga/traversal/kotlin::get-dot-expressions
              ;;         ↓
              ;; package p1.client
              (find-ast-in-ctx `((:path . ,path) (:line . 1) (:offset . 9)))))))))

(test find-signature-for-stdlib
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let* ((path "p1/client/ClientRestTemplate.kt")
           (actual (inga/traversal/kotlin::find-signature-for-stdlib "listOf" path)))
      (is (and (equal "kotlin.collections.CollectionsKt" (jsown:val actual "fqcn"))
               (equal "listOf" (jsown:val actual "name")))))))

