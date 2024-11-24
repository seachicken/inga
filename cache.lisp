(defpackage #:inga/cache
  (:use #:cl)
  (:export #:defunc))
(in-package #:inga/cache)

(defmacro defunc (name params &body body)
  (let ((cache-sym (gensym "CACHE-"))
        (required-params (loop for p in params
                               with is-optional
                               with results
                               do
                               (when (eq p '&optional) (setf is-optional t))
                               (unless is-optional (push p results))
                               finally (return (reverse results)))))
    `(progn
       (defparameter ,cache-sym (make-hash-table :test 'equal))
       (defun ,name ,params
         (if (equal (uiop:getenv "INGA_DEBUG") "1")
             (progn ,@body)
             (let* ((cache-key (list ,@required-params))
                    (cached-result (gethash cache-key ,cache-sym)))
               (if cached-result
                   (if (eq cached-result 'empty) nil cached-result)
                   (let ((result (progn ,@body)))
                     (setf (gethash cache-key ,cache-sym) (or result 'empty))
                     result)))))
       (defun ,(intern (concatenate 'string "EVICTC-" (string name))) ,required-params
         (unless (equal (uiop:getenv "INGA_DEBUG") "1")
           (remhash (list ,@required-params) ,cache-sym))))))

