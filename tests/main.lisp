(defpackage inga/tests/main
  (:use :cl
        :inga
        :fiveam))
(in-package :inga/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :inga)` in your Lisp.

(test 解析する
  (is (= 2 (inga:analyze))))
