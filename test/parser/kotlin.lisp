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
(test find-affected-poss-for-method
  (let ((parser (make-parser :java *fixtures-path* *cache*)))
    (start-parser parser '("*.kt") nil)
    (is (equal
          '(((:fq-name . "com.example.Class.method")
             (:name . "method")
             (:path . "declaration.kt")
             (:line . 6) (:offset . 5))) ;; FIXME: actual offset is 9
          (find-affected-poss
            parser
            '((:path . "declaration.kt")
              (:start . 7) (:end . 7)))))
    (stop-parser parser)))

(test find-references-to-imported-class
  (let ((parser (make-parser :java *jvm-path* *cache*)))
    (start-parser parser inga/main::*include-java* nil)
    (is (equal
          '(((:path . "Main.kt")
             (:line . 7) (:offset . 17))
            ((:path . "java/Class.java")
             (:line . 9) (:offset . 15)) ;; FIXME: should be 9, 16
            ((:path . "kotlin/a/PrimaryConstructor.kt")
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
             (:line . 8) (:offset . 30)))
          (find-references parser
                           '((:path . "kotlin/b/Class.kt")
                             (:name . "method")
                             (:line . 4) (:offset . 9)
                             (:fq-name . "jvm.kotlin.b.Class.method")))))
    (stop-parser parser)))

