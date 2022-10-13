(defpackage #:inga/errors
  (:use #:cl)
  (:export #:inga-error))
(in-package #:inga/errors)

(define-condition inga-error (error) ())

