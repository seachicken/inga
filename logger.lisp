(defpackage #:inga/logger
  (:nicknames #:logger)
  (:use #:cl)
  (:import-from #:local-time)
  (:import-from #:inga/contexts
                #:*mode*)
  (:export #:log-debug
           #:log-debug-generic
           #:log-info
           #:log-info-generic
           #:log-error
           #:log-error-generic))
(in-package #:inga/logger)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defparameter *logging-q* (sb-concurrency:make-queue))
(defparameter *logging-thread* nil)

(defun log-debug (content)
  (let ((debug (uiop:getenv "INGA_DEBUG")))
    (when (equal debug "1")
      (process-if-present (lambda () (log-debug-generic *mode* content))))))
(defgeneric log-debug-generic (mode content)
  (:method (mode content)
   (format t "~&~a DEBUG ~a~%" (local-time:now) content)))

(defun log-info (content)
  (process-if-present (lambda () (log-info-generic *mode* content))))
(defgeneric log-info-generic (mode content)
  (:method (mode content)
   (format t "~&~a INFO ~a~%" (local-time:now) content)))

(defun log-error (content)
  (process-if-present (lambda () (log-error-generic *mode* content))))
(defgeneric log-error-generic (mode content)
  (:method (mode content)
   (format *error-output* "~&~a ERROR ~a~%" (local-time:now) content)))

(defun process-if-present (fun)
  (when (or (not fun)
            (and *logging-thread* (sb-thread:thread-alive-p *logging-thread*)))
    (return-from process-if-present))
  
  (sb-concurrency:enqueue fun *logging-q*)
  (setf *logging-thread*
        (sb-thread:make-thread
          (lambda ()
            (funcall fun)
            (process-if-present (sb-concurrency:dequeue *logging-q*))))))

