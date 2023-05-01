(defpackage #:inga/test/parser/kotlin
  (:use #:cl
        #:fiveam
        #:inga/parser))
(in-package #:inga/test/parser/kotlin)

(def-suite kotlin)
(in-suite kotlin)

(defparameter *fixtures-path* (uiop:merge-pathnames* "test/fixtures/"))
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

(test find-references
  (let ((parser (make-parser :java *fixtures-path* *cache*)))
    (start-parser parser '("*.kt") nil)
    ;; TODO: find references
    (stop-parser parser)))

