(defpackage #:inga/test/cache
  (:use #:cl
        #:fiveam
        #:inga/cache))
(in-package #:inga/test/cache)

(def-suite cache)
(in-suite cache)

(test remove-least-recently-used-cache
  (is (equal
        nil
        (let ((cache (make-cache 2)))
          (put-value cache :a 'A)
          (put-value cache :b 'B)
          (put-value cache :c 'C)
          (get-value cache :a)))))

(test remove-frequently-used-cache
  (is (equal
        nil
        (let ((cache (make-cache 2)))
          (put-value cache :a 'A)
          (put-value cache :b 'B)
          (get-value cache :a)
          (put-value cache :c 'C)
          (get-value cache :b)))))

