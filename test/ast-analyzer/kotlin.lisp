(defpackage #:inga/test/ast-analyzer/kotlin
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer
        #:inga/ast-index
        #:inga/test/helper))
(in-package #:inga/test/ast-analyzer/kotlin)

(def-suite kotlin)
(in-suite kotlin)

(defparameter *kotlin-path* (merge-pathnames #p"test/fixtures/kotlin/"))

(test find-definitions-for-method
  (with-fixture jvm-context (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/PrimaryConstructorDefinition.kt")
             (:name . "method")
             (:fq-name . "p1.PrimaryConstructorDefinition.method")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/PrimaryConstructorDefinition.kt" *kotlin-path*)
                      '((:line . 4) (:offset . 5)))))) ;; FIXME: actual offset is 9
          (find-definitions (create-range "p1/PrimaryConstructorDefinition.kt" :line 4))))))

(test find-references-for-primary-constructor
  (with-fixture jvm-context (*kotlin-path* 'ast-index-disk)
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
  (with-fixture jvm-context (*kotlin-path* 'ast-index-disk)
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
  (with-fixture jvm-context (*kotlin-path* 'ast-index-disk)
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

