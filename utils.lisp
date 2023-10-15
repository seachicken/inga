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
(defun funtime (func &key label args)
  (log-debug (format nil "begin ~a args: ~a" label args))
  (let ((start-time (get-internal-real-time))
        (result (funcall func)))
    (log-debug (format nil "end ~a. time: ~,5f seconds"
                       label
                       (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
    result))

