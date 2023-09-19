(defpackage #:inga/errors
  (:use #:cl)
  (:export #:inga-error
           #:inga-error-process-not-running))
(in-package #:inga/errors)

(define-condition inga-error (error) ())
(define-condition inga-error-process-not-running (inga-error) ())

