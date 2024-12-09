(defpackage #:inga/test/analyzer/jvm-helper
  (:use #:cl
        #:fiveam
        #:inga/analyzer/jvm-helper))
(in-package #:inga/test/analyzer/jvm-helper)

(def-suite analyzer/jvm-helper)
(in-suite analyzer/jvm-helper)

(test get-exact-match-fq-class-name
  (is (equal
        '(("java.util.List") :exact)
        (multiple-value-list
          (get-fq-class-name-candidates "List" '("com.example.*" "java.util.List"))))))

(test get-fq-class-name-candidates-from-import
  (is (equal
        '(("com.fuzzy.List" "java.util.List") :fuzzy)
        (multiple-value-list
          (get-fq-class-name-candidates "List" '("com.example.Test" "com.fuzzy.*" "java.util.*"))))))

