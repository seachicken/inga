(defpackage #:inga/git
  (:use #:cl
        #:inga/utils)
  (:import-from #:cl-ppcre)
  (:import-from #:inga/errors
                #:inga-error)
  (:export #:diff-to-ranges))
(in-package #:inga/git)

(defun diff-to-ranges (diff)
  (let ((ranges (make-array 10 :fill-pointer 0 :adjustable t)) to-path)
    (with-input-from-string (in diff)
      (loop for line = (read-line in nil nil)
            while line
            do
            (let ((found-to-path (car (cdr (multiple-value-list
                                             (ppcre:scan-to-strings "^diff --git a/.+ b/(.+)$" line))))))
              (when found-to-path
                (setq to-path (aref found-to-path 0))))
            (let ((to-range (car (cdr (multiple-value-list
                                        (ppcre:scan-to-strings "@@.+\\+([0-9]+),?([0-9]*) @@" line))))))
              (when to-range
                (let ((start (parse-integer (aref to-range 0)))
                      (rows (if (> (length (aref to-range 1)) 0) (parse-integer (aref to-range 1)) 0)))
                  (let ((end (if (= rows 0) start (- (+ start rows) 1))))
                    ;; extract undeleted files
                    (when (or (> start 0) (> end 0))
                      (vector-push-extend (list (cons :path to-path)
                                                (cons :start start)
                                                (cons :end end))
                                          ranges))))))))
    (return-from diff-to-ranges (coerce ranges 'list))))

