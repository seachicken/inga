(defpackage #:inga/utils
  (:use #:cl)
  (:export #:make-queue
           #:enqueue
           #:dequeue))
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

;; for debug
(defun top (sequence limit)
  (let ((line-no 0) (result ""))
    (with-input-from-string (in sequence)
      (loop :for line := (read-line in nil nil) :while line
            :do (progn
                  (setf line-no (+ line-no 1))
                  (setf result (format nil "~a~a~%" result line))
                  (when (= line-no limit)
                    (return-from top result)))))))

