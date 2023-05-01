(defpackage #:inga/test/parser/kotlin
  (:use #:cl
        #:fiveam
        #:inga/parser))
(in-package #:inga/test/parser/kotlin)

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
(test find-affected-pos-for-method
  (let ((parser (make-parser :java *fixtures-path* *cache*)))
    (start-parser parser '("*.kt") nil)
    (is (equal
          '((:path . "declaration.kt")
            (:name . "method")
            (:line . 6) (:offset . 5) ;; FIXME: actual offset is 9
            (:fq-name . "com.example.Class.method"))
          (let ((src-path "declaration.kt"))
            (find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              7))))
    (stop-parser parser)))

(test find-references-to-imported-class
  (let ((parser (make-parser :java *jvm-path* *cache*)))
    (start-parser parser inga/main::*include-java* nil)
    (is (equal
          '(((:path . "Main.kt")
             (:name)
             (:line . 7) (:offset . 9)))
          (find-references parser
                           '((:path . "kotlin/a/Class.kt")
                             (:name . "method")
                             (:line . 4) (:offset . 9)
                             (:fq-name . "jvm.kotlin.a.Class.method")))))
    (stop-parser parser)))

(test find-references-to-fq-class
  (let ((parser (make-parser :java *jvm-path* *cache*)))
    (start-parser parser inga/main::*include-java* nil)
    (is (equal
          '(((:path . "Main.kt")
             (:name)
             (:line . 8) (:offset . 9)))
          (find-references parser
                           '((:path . "kotlin/b/Class.kt")
                             (:name . "method")
                             (:line . 4) (:offset . 9)
                             (:fq-name . "jvm.kotlin.b.Class.method")))))
    (stop-parser parser)))

