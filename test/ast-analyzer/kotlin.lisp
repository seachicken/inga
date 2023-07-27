(defpackage #:inga/test/ast-analyzer/kotlin
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer))
(in-package #:inga/test/ast-analyzer/kotlin)

(def-suite kotlin)
(in-suite kotlin)

(defparameter *test-path* (merge-pathnames #p"test/"))
(defparameter *cache* (inga/cache:make-cache 100))

(test find-definitions-for-method
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer '("*.kt") nil)
    (is (equal
          `(((:path . "fixtures/kotlin/PrimaryConstructorDefinition.kt")
             (:name . "method")
             (:fq-name . "fixtures.kotlin.PrimaryConstructorDefinition.method")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/kotlin/PrimaryConstructorDefinition.kt"
                      '((:line . 4) (:offset . 5)))))) ;; FIXME: actual offset is 9
          (find-definitions
            ast-analyzer
            `((:path . "fixtures/kotlin/PrimaryConstructorDefinition.kt")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *test-path* "fixtures/kotlin/PrimaryConstructorDefinition.kt"
                       '((:line . 5) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *test-path* "fixtures/kotlin/PrimaryConstructorDefinition.kt"
                       '((:line . 5) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-to-primary-constructor
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "fixtures/kotlin/PrimaryConstructorReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/kotlin/PrimaryConstructorReference.kt"
                      '((:line . 7) (:offset . 11))))))
          (find-references ast-analyzer
                           '((:path . "fixtures/kotlin/PrimaryConstructorHelper.kt")
                             (:name . "method")
                             (:line . 4) (:offset . 9)
                             (:fq-name . "fixtures.kotlin.PrimaryConstructorHelper.method")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-to-fq-method
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "fixtures/kotlin/FqMethodReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/kotlin/FqMethodReference.kt"
                      '((:line . 5) (:offset . 42))))))
          (find-references ast-analyzer
                           '((:path . "fixtures/kotlin/FqMethodHelper.kt")
                             (:name . "method")
                             (:line . 4) (:offset . 9)
                             (:fq-name . "fixtures.kotlin.FqMethodHelper.method")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-to-java-class
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "fixtures/kotlin/JavaReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/kotlin/JavaReference.kt"
                      '((:line . 7) (:offset . 11))))))
          (find-references ast-analyzer
                           '((:path . "fixtures/java/KotlinReference.java")
                             (:name . "method")
                             (:line . 8) (:offset . 17)
                             (:fq-name . "fixtures.java.KotlinReference.method")))))
    (stop-ast-analyzer ast-analyzer)))

