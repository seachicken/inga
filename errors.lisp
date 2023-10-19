(defpackage #:inga/errors
  (:use #:cl)
  (:export #:inga-error
           #:inga-error-process-not-running
           #:inga-error-process-failed))
(in-package #:inga/errors)

(define-condition inga-error (error) ())
(define-condition inga-error-process-not-running (inga-error) ())
(define-condition inga-error-process-failed (inga-error) ())

