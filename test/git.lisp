(defpackage :inga/test/git
  (:use :cl
        :fiveam
        :inga/git))
(in-package :inga/test/git)

(def-suite git)

(in-suite git)

(defparameter src-path "test/fixtures/react-typescript-todo")

(test get-diff
  (is (equal '(("start" . 13) ("end" . 15)
               ("start" . 23) ("end" . 23))
             (inga:get-diff "a690a51" "69bf5cb"))))

(test diffが1行のみ
  (is (equal '(("start" . 6) ("end" . 6))
             (inga:get-diff "a690a51" "dc33553"))))
 
(run! 'git)
;;(run! 'get-diff)
;;(run! 'diffが1行のみ)
