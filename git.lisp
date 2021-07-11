(defpackage :inga/git
  (:use :cl)
  (:export #:start-git #:stop-git))
(in-package :inga/git)

(defun start-git ()
  (print "start git")
  )

(defun stop-git ()
  (print "stop git")
  )

