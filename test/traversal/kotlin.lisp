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
          `(((:type . :module-default)
             (:path . "src/main/kotlin/p1/PrimaryConstructorDefinition.kt")
             (:name . "method")
             (:fq-name . "p1.PrimaryConstructorDefinition.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/p1/PrimaryConstructorDefinition.kt" *kotlin-path*)
                      '((:line . 4) (:offset . 5))))))
          (find-definitions (create-range "src/main/kotlin/p1/PrimaryConstructorDefinition.kt" :line 4))))))

(test find-definitions-for-value-class
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (is (equal
          `(((:type . :module-default)
             (:path . "src/main/kotlin/p1/ValueClassDefinition.kt")
             (:name . "ValueClass")
             (:fq-name . "p1.ValueClassDefinition$ValueClass.ValueClass-java.lang.String")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/p1/ValueClassDefinition.kt" *kotlin-path*)
                      '((:line . 5) (:offset . 27))))))
          (find-definitions (create-range "src/main/kotlin/p1/ValueClassDefinition.kt" :line 5))))))

(test find-references-for-primary-constructor
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "src/main/kotlin/p1/PrimaryConstructorReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/p1/PrimaryConstructorReference.kt" *kotlin-path*)
                      '((:line . 5) (:offset . 11))))))
          (find-references
            '((:path . "src/main/kotlin/p1/PrimaryConstructorHelper.kt")
              (:name . "method")
              (:fq-name . "p1.PrimaryConstructorHelper.method"))
            *index*)))))

(test find-references-for-fq-method
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "src/main/kotlin/p1/FqMethodReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/p1/FqMethodReference.kt" *kotlin-path*)
                      '((:line . 5) (:offset . 29))))))
          (find-references
            '((:path . "src/main/kotlin/p1/FqMethodHelper.kt")
              (:name . "method")
              (:fq-name . "p1.FqMethodHelper.method"))
            *index*)))))

(test find-references-for-java-class
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "src/main/kotlin/p1/JavaReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/p1/JavaReference.kt" *kotlin-path*)
                      '((:line . 5) (:offset . 11))))))
          (find-references
            '((:path . "src/main/kotlin/p1/KotlinReference.java")
              (:name . "method")
              (:fq-name . "p1.KotlinReference.method"))
            *index*)))))

(test find-fq-name-for-reference-with-enum
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/EnumReference.kt"))
      (is (equal
            "p1.EnumHelper.method-p1.EnumHelper.Enum"
            (find-fq-name
              ;;   ↓
              ;; v.method(Enum.A)
              (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 11)))
              path))))))

(test find-fq-name-for-reference-with-properties
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/PropertyReference.kt"))
      (is (equal
            "p1.PropertyHelper.method-java.lang.String"
            (find-fq-name
              (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 11)))
              path))))))

(test find-fq-name-for-reference-with-not-null-parameter
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/ParameterReference.kt"))
      (is (equal
            "p1.ParameterHelper.method"
            (find-fq-name
              ;; fun method(v: Helper) {
              ;;     ↓
              ;;   v.method()
              ;; }
              (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 11)))
              path))))))

(test find-fq-name-for-reference-with-nullable-parameter
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/ParameterReference.kt"))
      (is (equal
            "p1.ParameterHelper.method"
            (find-fq-name
              ;; fun method(v: Helper?) {
              ;;      ↓
              ;;   v?.method()
              ;; }
              (find-ast-in-ctx `((:path . ,path) (:line . 9) (:offset . 12)))
              path))))))

(test find-fq-name-for-reference-with-vararg
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/VarargReference.kt"))
      (is (equal
            "p1.VarargReference.method-p1.VarargHelper"
            (find-fq-name
              ;; fun method(vararg vs: Helper) {
              ;;   ↓
              ;;   method(vs)
              ;; }
              (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 9)))
              path))))))

(test find-fq-name-for-reference-with-anonymous-objects
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/AnonymousObjectReference.kt"))
      (is (equal
            "java.lang.Thread.Thread-java.lang.Runnable"
            (find-fq-name
              ;; ↓
              ;; Thread(object : Runnable {
              (first (get-asts (find-ast-in-ctx `((:path . ,path) (:line . 8) (:offset . 9)))
                               '("CALL_EXPRESSION")))
              path))))))

(test find-fq-name-for-reference-with-anonymous-objects-super-call
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/AnonymousObjectReference.kt"))
      (is (equal
            "p1.AnonymousObjectReference.anonymousMethod-p1.AnonymousObjectHelper"
            (find-fq-name
              ;; ↓
              ;; anonymousMethod(object : AnonymousObjectHelper() {})
              (find-ast-in-ctx `((:path . ,path) (:line . 15) (:offset . 9)))
              path))))))

(test find-fq-name-for-method-chain-first
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/TypeInferenceReference.kt"))
      (is (equal
            "kotlin.collections.CollectionsKt.listOf-java.lang.String"
            (find-fq-name
              ;; ↓
              ;; listOf("a").forEach { println(it) }
              (first (get-asts
                       (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 9)))
                       '("CALL_EXPRESSION")))
              path))))))

(test find-fq-name-for-value-class
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/ValueClassReference.kt"))
      (is (equal
            "p1.ValueClassReferenceHelper$ValueClass.ValueClass-java.lang.String"
            (find-fq-name
              ;; ↓
              ;; ValueClass("a")
              (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 9)))
              path))))))

(test find-fq-name-for-method-chain-second
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/TypeInferenceReference.kt"))
      (is (equal
            "kotlin.collections.CollectionsKt.forEach-FUNCTION_LITERAL"
            (find-fq-name
              ;;             ↓
              ;; listOf("a").forEach { println(it) }
              (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 21)))
              path))))))

(test find-fq-class-name-for-type-inference-in-lambda
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/TypeInferenceReference.kt"))
      (is (equal
            "java.lang.String"
            ;;                               ↓
            ;; listOf("a").forEach { println(it) }
            (find-fq-class-name
              (first (get-asts
                       (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 39)))
                       '("REFERENCE_EXPRESSION")))
              path))))))

(test find-fq-class-name-for-reference-type-inference-in-lambda
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/TypeInferenceReference.kt"))
      (is (equal
            "java.lang.String"
            (find-fq-class-name
              (first (get-asts
                       (find-ast-in-ctx `((:path . ,path) (:line . 10) (:offset . 32)))
                       '("REFERENCE_EXPRESSION")))
              path))))))

(test get-scope-with-private
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/VisibilityPrivateDefinition.kt"))
      (is (equal
            :module-private
            (inga/traversal/kotlin::get-scope
              ;;             ↓
              ;; private void method() {
              (find-ast-in-ctx `((:path . ,path) (:line . 4) (:offset . 17)))))))))

(test get-dot-expressions-with-zero-dot
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/PrimaryConstructorDefinition.kt"))
      (is (equal
            '("p1")
            (inga/traversal/kotlin::get-dot-expressions
              ;;         ↓
              ;; package p1
              (find-ast-in-ctx `((:path . ,path) (:line . 1) (:offset . 9)))))))))

(test get-dot-expressions-with-two-dots
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let ((path "src/main/kotlin/p1/p2/p3/PackageDefinition.kt"))
      (is (equal
            '("p1" "p2" "p3")
            (inga/traversal/kotlin::get-dot-expressions
              ;;         ↓
              ;; package p1.p2.p3
              (find-ast-in-ctx `((:path . ,path) (:line . 1) (:offset . 9)))))))))

(test find-signature-for-stdlib
  (with-fixture jvm-ctx (*kotlin-path* 'ast-index-memory)
    (let* ((path "src/main/kotlin/p1/KotlinReference.kt")
           (actual (inga/traversal/kotlin::find-signature-for-stdlib "listOf" path)))
      (is (and (equal "kotlin.collections.CollectionsKt" (jsown:val actual "fqcn"))
               (equal "listOf" (jsown:val actual "name")))))))

