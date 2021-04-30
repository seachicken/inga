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

(test moduleは必ず行に含まれるので無視する
  (is-false (inga::contains-line
             '(:OBJ ("text" . "\"index\"")
                    ("kind" . "module")
                    ("spans"
                     (:OBJ ("start" :OBJ ("line" . 1) ("offset" . 1))
                      ("end" :OBJ ("line" . 5) ("offset" . 1))))
                    ("childItems"
                     (:OBJ ("text" . "addTodo")
                      ("kind" . "function")
                      ("spans"
                       (:OBJ ("start" :OBJ ("line" . 1) ("offset" . 1))
                        ("end" :OBJ ("line" . 3) ("offset" . 1)))))))
             2)))

(test functionが含まれる行であればtrueを返す
  (is-true (inga::contains-line
             '(:OBJ ("text" . "addTodo")
                      ("kind" . "function")
                      ("spans"
                       (:OBJ ("start" :OBJ ("line" . 1) ("offset" . 1))
                        ("end" :OBJ ("line" . 3) ("offset" . 1)))))
             2)))

(run! 'main)

