(defpackage #:inga/cache
  (:use #:cl)
  (:export #:defunc))
(in-package #:inga/cache)

(defmacro defunc (name params &body body)
  (let ((cache-sym (gensym "CACHE-")))
    `(progn
       (defparameter ,cache-sym (make-hash-table :test 'equal))
       (defun ,name ,params
         (if (equal (uiop:getenv "INGA_DEBUG") "1")
             (progn ,@body)
             (let* ((cache-key (list ,@params))
                    (cached-result (gethash cache-key ,cache-sym)))
               (if cached-result
                   (if (eq cached-result 'empty) nil cached-result)
                   (let ((result (progn ,@body)))
                     (setf (gethash cache-key ,cache-sym) (or result 'empty))
                     result)))))
       (defun ,(intern (concatenate 'string "EVICTC-" (string name))) ,params
         (remhash (list ,@params) ,cache-sym)))))

