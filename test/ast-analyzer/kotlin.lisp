(defpackage #:inga/test/ast-analyzer/kotlin
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer))
(in-package #:inga/test/ast-analyzer/kotlin)

(def-suite kotlin)
(in-suite kotlin)

(defparameter *test-path* (merge-pathnames #p"test/"))
(defparameter *fixtures-path* (merge-pathnames #p"test/fixtures/"))
(defparameter *jvm-path* (merge-pathnames #p"test/fixtures/jvm/"))
(defparameter *cache* (inga/cache:make-cache 100))

;; class Class {
;;     val field = 0
;; 
;;         ↓[out]
;;     fun method(): Int {
;;       return 0 ←[in]
;;     }
;; }
(test find-definitions-for-method
  (let ((ast-analyzer (make-ast-analyzer :java *fixtures-path* *cache*)))
    (start-ast-analyzer ast-analyzer '("*.kt") nil)
    (is (equal
          `(((:path . "declaration.kt")
             (:name . "method")
             (:fq-name . "com.example.Class.method")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *fixtures-path* "declaration.kt"
                      '((:line . 6) (:offset . 5)))))) ;; FIXME: actual offset is 9
          (find-definitions
            ast-analyzer
            `((:path . "declaration.kt")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *fixtures-path* "declaration.kt"
                       '((:line . 7) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *fixtures-path* "declaration.kt"
                       '((:line . 7) (:offset . -1))))))))
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

(test find-references-to-fq-class
  (let ((ast-analyzer (make-ast-analyzer :java *jvm-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "Main.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path* "Main.kt"
                      '((:line . 8) (:offset . 30))))))
          (find-references ast-analyzer
                           '((:path . "kotlin/b/Class.kt")
                             (:name . "method")
                             (:fq-name . "jvm.kotlin.b.Class.method")))))
    (stop-ast-analyzer ast-analyzer)))

