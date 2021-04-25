(defpackage inga/tests/main
  (:use :cl
        :common-lisp
        :inga
        :fiveam))
(in-package :inga/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :inga)' in your Lisp.

(def-suite main)

(in-suite main)

(test 解析する
  (inga:start)
  (is (= 2 (inga:analyze)))
  (inga:stop))

(run! 'main)

