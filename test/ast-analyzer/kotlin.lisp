(defpackage #:inga/test/ast-analyzer/kotlin
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer))
(in-package #:inga/test/ast-analyzer/kotlin)

(def-suite kotlin)
(in-suite kotlin)

(defparameter *kotlin-path* (merge-pathnames #p"test/fixtures/kotlin/"))
(defparameter *cache* (inga/cache:make-cache 100))

(test find-definitions-for-method
  (let ((ast-analyzer (make-ast-analyzer :java *kotlin-path* *cache*)))
    (start-ast-analyzer ast-analyzer '("*.kt") nil)
    (is (equal
          `(((:path . "p1/PrimaryConstructorDefinition.kt")
             (:name . "method")
             (:fq-name . "p1.PrimaryConstructorDefinition.method")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *kotlin-path* "p1/PrimaryConstructorDefinition.kt"
                      '((:line . 4) (:offset . 5)))))) ;; FIXME: actual offset is 9
          (find-definitions
            ast-analyzer
            `((:path . "p1/PrimaryConstructorDefinition.kt")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *kotlin-path* "p1/PrimaryConstructorDefinition.kt"
                       '((:line . 4) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *kotlin-path* "p1/PrimaryConstructorDefinition.kt"
                       '((:line . 4) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-for-primary-constructor
  (let ((ast-analyzer (make-ast-analyzer :java *kotlin-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "p1/PrimaryConstructorReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *kotlin-path* "p1/PrimaryConstructorReference.kt"
                      '((:line . 7) (:offset . 11))))))
          (find-references ast-analyzer
                           '((:path . "p1/PrimaryConstructorHelper.kt")
                             (:name . "method")
                             (:fq-name . "p1.PrimaryConstructorHelper.method")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-for-fq-method
  (let ((ast-analyzer (make-ast-analyzer :java *kotlin-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "p1/FqMethodReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *kotlin-path* "p1/FqMethodReference.kt"
                      '((:line . 5) (:offset . 29))))))
          (find-references ast-analyzer
                           '((:path . "p1/FqMethodHelper.kt")
                             (:name . "method")
                             (:fq-name . "p1.FqMethodHelper.method")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-for-java-class
  (let ((ast-analyzer (make-ast-analyzer :java *kotlin-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "p1/JavaReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *kotlin-path* "p1/JavaReference.kt"
                      '((:line . 7) (:offset . 11))))))
          (find-references ast-analyzer
                           '((:path . "p1/KotlinReference.java")
                             (:name . "method")
                             (:fq-name . "p1.KotlinReference.method")))))
    (stop-ast-analyzer ast-analyzer)))

