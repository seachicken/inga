(defpackage #:inga/cache
  (:use #:cl)
  (:export #:defunc
           #:cache
           #:make-cache
           #:put-value
           #:get-value
           #:size))
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

(defclass cache ()
  ((max-size
     :initarg :max-size
     :accessor cache-max-size)
   (store
     :initform nil
     :accessor cache-store)))

(defmethod make-cache (max-size)
  (make-instance 'cache
                 :max-size max-size))

(defmethod put-value ((cache cache) key value)
  (when (>= (length (cache-store cache)) (cache-max-size cache))
    (nbutlast (cache-store cache) (+ (- (length (cache-store cache)) (cache-max-size cache)) 1)))
  (push (cons key value) (cache-store cache)))

(defmethod get-value ((cache cache) key)
  (let ((pair (assoc key (cache-store cache))))
    (when pair
      (delete pair (cache-store cache))
      (push pair (cache-store cache))
      (cdr pair))))

(defmethod size ((cache cache))
  (length (cache-store cache)))

