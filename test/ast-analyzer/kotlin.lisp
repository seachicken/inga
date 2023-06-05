(defpackage #:inga/test/ast-analyzer/kotlin
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer))
(in-package #:inga/test/ast-analyzer/kotlin)

(def-suite kotlin)
(in-suite kotlin)

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

(test find-references-to-imported-class
  (let ((ast-analyzer (make-ast-analyzer :java *jvm-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "Main.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path* "Main.kt"
                      '((:line . 7) (:offset . 17)))))
            ((:path . "java/Class.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path* "java/Class.java"
                      '((:line . 10) (:offset . 15)))))
            ((:path . "java/Class.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path* "java/Class.java"
                      '((:line . 11) (:offset . 20)))))
            ((:path . "java/Class.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path* "java/Class.java"
                      '((:line . 17) (:offset . 20)))))
            ((:path . "kotlin/a/PrimaryConstructor.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path* "kotlin/a/PrimaryConstructor.kt"
                      '((:line . 7) (:offset . 9))))))
          (find-references ast-analyzer
                           '((:path . "kotlin/a/Class.kt")
                             (:name . "method")
                             (:line . 4) (:offset . 9)
                             (:fq-name . "jvm.kotlin.a.Class.method")))))
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

