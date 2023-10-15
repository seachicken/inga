(defpackage #:inga/cache
  (:use #:cl)
  (:export #:defunc))
(in-package #:inga/cache)

(defmacro defunc (name params &body body)
  (let ((cache-sym (gensym "CACHE-")))
    `(progn
       (defparameter ,cache-sym (make-hash-table :test 'equal))
       (defun ,name ,params
         (let* ((args (list ,@params))
                (cache-key (list ,@params))
                (cached-result (gethash cache-key ,cache-sym)))
           (if cached-result
               cached-result
               (let ((result (progn ,@body)))
                 (setf (gethash cache-key ,cache-sym) result))))))))

