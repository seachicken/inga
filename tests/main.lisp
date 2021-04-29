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
  (is (equal '(:pos ("line" . 11) ("offset" . 12)) (inga:analyze)))
  (inga:stop))

;;(test contains-line
;;  (is-true (inga::contains-line '((OBJ (start OBJ (line . 2) (offset . 1))
;;                                        (end OBJ (line . 3) (offset . 2))))
;;                                2)))

(run! 'main)

