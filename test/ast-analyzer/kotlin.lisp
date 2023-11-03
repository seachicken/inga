(defpackage #:inga/test/ast-analyzer/kotlin
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer))
(in-package #:inga/test/ast-analyzer/kotlin)

(def-suite kotlin)
(in-suite kotlin)

(defparameter *kotlin-path* (merge-pathnames #p"test/fixtures/kotlin/"))

(test find-definitions-for-method
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *kotlin-path*)
            (start-ast-analyzer :kotlin nil *kotlin-path*))))
    (create-indexes *kotlin-path* :include inga/main::*include-java*)
    (is (equal
          `(((:path . "p1/PrimaryConstructorDefinition.kt")
             (:name . "method")
             (:fq-name . "p1.PrimaryConstructorDefinition.method")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/PrimaryConstructorDefinition.kt" *kotlin-path*)
                      '((:line . 4) (:offset . 5)))))) ;; FIXME: actual offset is 9
          (find-definitions
            `((:path . "p1/PrimaryConstructorDefinition.kt")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/PrimaryConstructorDefinition.kt" *kotlin-path*)
                       '((:line . 4) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/PrimaryConstructorDefinition.kt" *kotlin-path*)
                       '((:line . 4) (:offset . -1))))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-references-for-primary-constructor
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *kotlin-path*)
            (start-ast-analyzer :kotlin nil *kotlin-path*))))
    (create-indexes *kotlin-path* :include inga/main::*include-java*)
    (is (equal
          `(((:path . "p1/PrimaryConstructorReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/PrimaryConstructorReference.kt" *kotlin-path*)
                      '((:line . 7) (:offset . 11))))))
          (find-references
            '((:path . "p1/PrimaryConstructorHelper.kt")
              (:name . "method")
              (:fq-name . "p1.PrimaryConstructorHelper.method")))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-references-for-fq-method
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *kotlin-path*)
            (start-ast-analyzer :kotlin nil *kotlin-path*))))
    (create-indexes *kotlin-path* :include inga/main::*include-java*)
    (is (equal
          `(((:path . "p1/FqMethodReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/FqMethodReference.kt" *kotlin-path*)
                      '((:line . 5) (:offset . 29))))))
          (find-references
            '((:path . "p1/FqMethodHelper.kt")
              (:name . "method")
              (:fq-name . "p1.FqMethodHelper.method")))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-references-for-java-class
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *kotlin-path*)
            (start-ast-analyzer :kotlin nil *kotlin-path*))))
    (create-indexes *kotlin-path* :include inga/main::*include-java*)
    (is (equal
          `(((:path . "p1/JavaReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/JavaReference.kt" *kotlin-path*)
                      '((:line . 7) (:offset . 11))))))
          (find-references
            '((:path . "p1/KotlinReference.java")
              (:name . "method")
              (:fq-name . "p1.KotlinReference.method")))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

