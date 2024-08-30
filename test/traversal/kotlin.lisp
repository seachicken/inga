(defpackage #:inga/test/traversal/kotlin
  (:use #:cl
        #:fiveam
        #:inga/traversal
        #:inga/ast-index
        #:inga/test/helper))
(in-package #:inga/test/traversal/kotlin)

(def-suite kotlin)
(in-suite kotlin)

(defparameter *kotlin-path* (merge-pathnames "test/fixtures/general/"))

(test find-definitions-for-method
  (with-fixture jvm-ctx (*kotlin-path*)
    (is (equal
          `(((:type . :module-default)
             (:path . "src/main/kotlin/pkt1/PrimaryConstructorDefinition.kt")
             (:name . "method")
             (:fq-name . "pkt1.PrimaryConstructorDefinition.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/pkt1/PrimaryConstructorDefinition.kt" *kotlin-path*)
                      '((:line . 4) (:offset . 5))))))
          (find-definitions (create-range "src/main/kotlin/pkt1/PrimaryConstructorDefinition.kt" :line 4))))))

(test find-definitions-for-value-class
  (with-fixture jvm-ctx (*kotlin-path*)
    (is (equal
          `(((:type . :module-default)
             (:path . "src/main/kotlin/pkt1/ValueClassDefinition.kt")
             (:name . "ValueClass")
             (:fq-name . "pkt1.ValueClassDefinition$ValueClass.ValueClass-java.lang.String")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/pkt1/ValueClassDefinition.kt" *kotlin-path*)
                      '((:line . 5) (:offset . 27))))))
          (find-definitions (create-range "src/main/kotlin/pkt1/ValueClassDefinition.kt" :line 5))))))

(test find-definition-of-field-reference
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/FieldDefinition.kt"))
      (is (equal
            ;;             ↓
            ;; private var field: String = ""
            (convert-to-top-offset
              (merge-pathnames path *kotlin-path*) '((:line . 4) (:offset . 17)))
            ;;      ↓
            ;; this.field = p
            (ast-value
              (inga/traversal/kotlin::find-definition
                "field"
                (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 14))))
              "textOffset"))))))

(test find-definition-of-parameter
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/FieldDefinition.kt"))
      (is (equal
            ;;            ↓
            ;; fun method(p: String) {
            (convert-to-top-offset
              (merge-pathnames path *kotlin-path*) '((:line . 6) (:offset . 16)))
            ;;              ↓
            ;; this.field = p
            (ast-value
              (inga/traversal/kotlin::find-definition
                "p"
                (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 22))))
              "textOffset"))))))

(test find-definition-of-local-variable
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/ObjectCallReference.kt"))
      (is (equal
            ;;     ↓
            ;; val client = ObjectCallHelper()
            (convert-to-top-offset
              (merge-pathnames path *kotlin-path*) '((:line . 5) (:offset . 13)))
            ;; ↓
            ;; client.method()
            (ast-value
              (inga/traversal/kotlin::find-definition
                "client"
                (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 9))))
              "textOffset"))))))

(test find-references-for-primary-constructor
  (with-fixture jvm-ctx (*kotlin-path*)
    (is (equal
          `(((:path . "src/main/kotlin/pkt1/PrimaryConstructorReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/pkt1/PrimaryConstructorReference.kt" *kotlin-path*)
                      '((:line . 5) (:offset . 9))))))
          (find-references
            '((:path . "src/main/kotlin/pkt1/PrimaryConstructorHelper.kt")
              (:name . "PrimaryConstructorHelper")
              (:fq-name . "pkt1.PrimaryConstructorHelper.PrimaryConstructorHelper-INT"))
            *index*)))))

(test find-references-for-fq-method
  (with-fixture jvm-ctx (*kotlin-path*)
    (is (equal
          `(((:path . "src/main/kotlin/pkt1/FqMethodReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/pkt1/FqMethodReference.kt" *kotlin-path*)
                      '((:line . 5) (:offset . 31))))))
          (find-references
            '((:path . "src/main/kotlin/pkt1/FqMethodHelper.kt")
              (:name . "method")
              (:fq-name . "pkt1.FqMethodHelper.method"))
            *index*)))))

(test find-references-for-java-class
  (with-fixture jvm-ctx (*kotlin-path*)
    (is (equal
          `(((:path . "src/main/kotlin/pkt1/JavaReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/kotlin/pkt1/JavaReference.kt" *kotlin-path*)
                      '((:line . 7) (:offset . 11))))))
          (find-references
            '((:path . "src/main/java/p1/KotlinReference.java")
              (:name . "method")
              (:fq-name . "p1.KotlinReference.method"))
            *index*)))))

(test find-reference-to-literal-field
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/LiteralReference.kt"))
      (is (equal
            `(((:path . ,path)
               (:name . "field literal")
               (:top-offset .
                ,(convert-to-top-offset
                   (merge-pathnames path *kotlin-path*) '((:line . 12) (:offset . 37))))))
            (find-reference-to-literal
              ;;        ↓
              ;; method(literal);
              (first (get-asts
                       (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 16)))
                       '("REFERENCE_EXPRESSION")))
              path))))))

(test find-caller
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/ObjectCallReference.kt"))
      (is (equal
            ;;        ↓
            ;; client.methodSelf()
            ;; client.method()
            (convert-to-top-offset
              (merge-pathnames path *kotlin-path*) '((:line . 6) (:offset . 16)))
            (ast-value
              (find-caller
                '(((:fq-name . "pkt1.ObjectCallHelper.methodSelf")))
                ;; client.methodSelf()
                ;;        ↓
                ;; client.method()
                (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 16)))
                path)
              "textOffset"))))))

(test find-caller-with-method-chains
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/ObjectCallReference.kt"))
      (is (equal
            ;;        ↓
            ;; client.methodSelf()
            ;;       .method()
            (convert-to-top-offset
              (merge-pathnames path *kotlin-path*) '((:line . 12) (:offset . 16)))
            (ast-value
              (find-caller
                '(((:fq-name . "pkt1.ObjectCallHelper.methodSelf")))
                ;; client.methodSelf()
                ;;        ↓
                ;;       .method()
                (find-ast-in-ctx `((:path . ,path) (:line . 13) (:offset . 14)))
                path)
              "textOffset"))))))

(test find-fq-name-for-reference-with-enum
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/EnumReference.kt"))
      (is (equal
            "pkt1.EnumHelper.method-pkt1.EnumHelper.Enum"
            (find-fq-name
              ;;   ↓
              ;; v.method(Enum.A)
              (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 11)))
              path))))))

(test find-fq-name-for-reference-with-properties
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/PropertyReference.kt"))
      (is (equal
            "pkt1.PropertyHelper.method-java.lang.String"
            (find-fq-name
              (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 11)))
              path))))))

(test find-fq-name-for-reference-with-not-null-parameter
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/ParameterReference.kt"))
      (is (equal
            "pkt1.ParameterHelper.method"
            (find-fq-name
              ;; fun method(v: Helper) {
              ;;     ↓
              ;;   v.method()
              ;; }
              (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 11)))
              path))))))

(test find-fq-name-for-reference-with-nullable-parameter
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/ParameterReference.kt"))
      (is (equal
            "pkt1.ParameterHelper.method"
            (find-fq-name
              ;; fun method(v: Helper?) {
              ;;      ↓
              ;;   v?.method()
              ;; }
              (find-ast-in-ctx `((:path . ,path) (:line . 9) (:offset . 12)))
              path))))))

(test find-fq-name-for-reference-with-vararg
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/VarargReference.kt"))
      (is (equal
            "pkt1.VarargReference.method-pkt1.VarargHelper"
            (find-fq-name
              ;; fun method(vararg vs: Helper) {
              ;;   ↓
              ;;   method(vs)
              ;; }
              (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 9)))
              path))))))

(test find-fq-name-for-reference-with-anonymous-objects
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/AnonymousObjectReference.kt"))
      (is (equal
            "java.lang.Thread.Thread-java.lang.Runnable"
            (find-fq-name
              ;; ↓
              ;; Thread(object : Runnable {
              (first (get-asts (find-ast-in-ctx `((:path . ,path) (:line . 8) (:offset . 9)))
                               '("CALL_EXPRESSION")))
              path))))))

(test find-fq-name-for-reference-with-anonymous-objects-super-call
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/AnonymousObjectReference.kt"))
      (is (equal
            "pkt1.AnonymousObjectReference.anonymousMethod-pkt1.AnonymousObjectHelper"
            (find-fq-name
              ;; ↓
              ;; anonymousMethod(object : AnonymousObjectHelper() {})
              (find-ast-in-ctx `((:path . ,path) (:line . 15) (:offset . 9)))
              path))))))

(test find-fq-name-for-value-class
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/ValueClassReference.kt"))
      (is (equal
            "pkt1.ValueClassReferenceHelper$ValueClass.ValueClass-java.lang.String"
            (find-fq-name
              ;; ↓
              ;; ValueClass("a")
              (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 9)))
              path))))))

(test find-fq-name-with-method-chain-first
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/TypeInferenceReference.kt"))
      (is (equal
            "kotlin.collections.CollectionsKt.listOf-java.lang.String"
            (find-fq-name
              ;; ↓
              ;; listOf("a").forEach { println(it) }
              (first (get-asts
                       (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 9)))
                       '("CALL_EXPRESSION")))
              path))))))

(test find-fq-name-with-method-chain-second
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/TypeInferenceReference.kt"))
      (is (equal
            "kotlin.collections.CollectionsKt.forEach-FUNCTION_LITERAL"
            (find-fq-name
              ;;             ↓
              ;; listOf("a").forEach { println(it) }
              (find-ast-in-ctx `((:path . ,path) (:line . 5) (:offset . 21)))
              path))))))

(test find-fq-name-with-method-chain-third
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/ObjectCallReference.kt"))
      (is (equal
            "pkt1.ObjectCallHelper.method"
            (find-fq-name
              ;;                     ↓
              ;; client.methodSelf().method()
              (find-ast-in-ctx `((:path . ,path) (:line . 13) (:offset . 14)))
              path))))))

(test find-fq-name-with-method-chain-nullable-higher-order-functions
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/ObjectCallReference.kt"))
      (is (equal
            "pkt1.ObjectCallHelper.methodWithHigherOrderFunctions-FUNCTION_LITERAL"
            (find-fq-name
              ;;                      ↓
              ;; client.methodSelf()?.method {}
              (find-ast-in-ctx `((:path . ,path) (:line . 19) (:offset . 15)))
              path))))))

(test find-fq-class-name-for-type-inference-in-lambda
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/TypeInferenceReference.kt"))
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
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/TypeInferenceReference.kt"))
      (is (equal
            "java.lang.String"
            (find-fq-class-name
              (first (get-asts
                       (find-ast-in-ctx `((:path . ,path) (:line . 10) (:offset . 32)))
                       '("REFERENCE_EXPRESSION")))
              path))))))

(test get-scope-with-private
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/VisibilityPrivateDefinition.kt"))
      (is (equal
            :module-private
            (inga/traversal/kotlin::get-scope
              ;;             ↓
              ;; private void method() {
              (find-ast-in-ctx `((:path . ,path) (:line . 4) (:offset . 17)))))))))

(test get-dot-expressions-with-zero-dot
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/PrimaryConstructorDefinition.kt"))
      (is (equal
            '("pkt1")
            (inga/traversal/kotlin::get-dot-expressions
              ;;         ↓
              ;; package pkt1
              (find-ast-in-ctx `((:path . ,path) (:line . 1) (:offset . 9)))))))))

(test get-dot-expressions-with-two-dots
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/p2/p3/PackageDefinition.kt"))
      (is (equal
            '("pkt1" "p2" "p3")
            (inga/traversal/kotlin::get-dot-expressions
              ;;         ↓
              ;; package pkt1.p2.p3
              (find-ast-in-ctx `((:path . ,path) (:line . 1) (:offset . 9)))))))))

(test find-signature-for-stdlib
  (with-fixture jvm-ctx (*kotlin-path*)
    (let ((path "src/main/kotlin/pkt1/KotlinReference.kt"))
      (is (equal
            "kotlin.collections.CollectionsKt"
            (inga/traversal/kotlin::find-signature-for-stdlib "listOf" path))))))

