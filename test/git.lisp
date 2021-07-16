(defpackage :inga/test/git
  (:use :cl
        :fiveam
        :inga/git))
(in-package :inga/test/git)

(def-suite git)

(in-suite git)

(test start-git
  (inga:start-git)
  (is (equal 0 0))
  )

(run! 'git)
