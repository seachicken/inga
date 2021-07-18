(defpackage :inga/test/git
  (:use :cl
        :fiveam
        :inga/git))
(in-package :inga/test/git)

(def-suite git)

(in-suite git)

(test get-diff
  (is (equal '(("start" . 13) ("end" . 15)
               ("start" . 23) ("end" . 23))
             (inga:get-diff "a690a51"))))
 
(run! 'git)
