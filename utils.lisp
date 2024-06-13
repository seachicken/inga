(defpackage #:inga/utils
  (:use #:cl)
  (:import-from #:inga/logger
                #:log-debug)
  (:export #:make-queue
           #:enqueue
           #:dequeue
           #:dequeue-last
           #:peek
           #:peek-last
           #:split
           #:split-trim-comma
           #:funtime))
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

(defun dequeue-last (q)
  (unless (null (queue-values q))
    (let ((last (last (queue-values q))))
      (setf (queue-values q) (butlast (queue-values q)))
      last)))

(defun peek (q)
  (unless (null (queue-values q))
    (first (queue-values q))))

(defun peek-last (q)
  (unless (null (queue-values q))
    (first (last (queue-values q)))))

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

(defun funtime (func &key label args (max-sec 0.5))
  (let* ((start-time (get-internal-real-time))
         (result (funcall func))
         (sec (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
    (when (> sec max-sec)
      (log-debug (format nil "~a time: ~,5f seconds, args: ~a" label sec args)))
    result))

