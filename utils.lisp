(defpackage #:inga/utils
  (:use #:cl)
  (:import-from #:inga/logger
                #:log-debug)
  (:export #:make-queue
           #:enqueue
           #:dequeue
           #:split
           #:split-trim-comma))
(in-package #:inga/utils)

(defstruct queue
  (values nil))

(defun enqueue (q v)
  (if (null (queue-values q))
      (setf (queue-values q) (list v))
      (nconc (queue-values q) (list v))))

(defun dequeue (q)
  (unless (null (queue-values q))
    (pop (queue-values q))))

(defun split (div sequence)
  (let ((pos (position div sequence)))
    (if pos
        (list* (subseq sequence 0 pos)
               (split div (subseq sequence (1+ pos))))
        (list sequence))))

(defun split-trim-comma (sequence)
  (mapcar (lambda (str)
            (string-trim '(#\Space) str))
          (split #\, sequence)))

;; for debug
(defstruct measuring-time
  (times 0)
  (cache-hit 0)
  (total-time 0))

(defun top (sequence limit)
  (let ((line-no 0) (result ""))
    (with-input-from-string (in (format nil "~a" sequence))
      (loop :for line := (read-line in nil nil) :while line
            :do (progn
                  (setf line-no (+ line-no 1))
                  (setf result (format nil "~a~a~%" result line))
                  (when (= line-no limit)
                    (return-from top result)))))))

(defun funtime (label func)
  (log-debug (format nil "begin ~a call...~%" label))
  (let ((start-time (get-internal-real-time))
        (result (funcall func)))
    (log-debug (format nil "end ~a. time: ~,5f seconds~%"
                       label
                       (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
    result))

(defun measure (target func)
  (let ((start-time (get-internal-real-time)))
    (multiple-value-bind (result cache-hit) (funcall func)
      (if cache-hit
          (setf (measuring-time-cache-hit target)
                (+ (measuring-time-cache-hit target) 1))
          (progn
            (setf (measuring-time-total-time target)
                  (+ (measuring-time-total-time target) (- (get-internal-real-time) start-time)))
            (setf (measuring-time-times target)
                  (+ (measuring-time-times target) 1))))
      result)))

(defun avg-sec (time)
  (if (= (measuring-time-times time) 0)
      0
      (/ (measuring-time-total-time time) (measuring-time-times time)
         internal-time-units-per-second)))

