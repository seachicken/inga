(defpackage #:inga/cache
  (:use #:cl)
  (:export #:cache
           #:make-cache
           #:put-value
           #:get-value))
(in-package #:inga/cache)

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
  (when (= (length (cache-store cache)) (cache-max-size cache))
    (nbutlast (cache-store cache) (- (cache-max-size cache) 1)))
  (push (cons key value) (cache-store cache)))

(defmethod get-value ((cache cache) key)
  (let ((pair (assoc key (cache-store cache))))
    (when pair
      (delete pair (cache-store cache))
      (push pair (cache-store cache))
      (cdr pair))))

