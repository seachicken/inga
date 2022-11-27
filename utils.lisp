(defpackage #:inga/utils
  (:use #:cl)
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

(defun measure (target func)
  (let ((start-time (get-internal-real-time)))
    (setf (measuring-time-times target)
          (+ (measuring-time-times target) 1))
    (prog1
      (funcall func)
      (setf (measuring-time-total-time target)
            (+ (measuring-time-total-time target) (- (get-internal-real-time) start-time))))))

