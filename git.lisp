(defpackage #:inga/git
  (:use #:cl
        #:inga/utils)
  (:import-from #:cl-ppcre)
  (:import-from #:inga/errors
                #:inga-error)
  (:import-from #:inga/file
                #:convert-to-top-offset)
  (:export #:diff-to-ranges))
(in-package #:inga/git)

(defun diff-to-ranges (diff root-path)
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
                      (vector-push-extend `((:path . ,to-path)
                                            (:start-offset .
                                             ,(convert-to-top-offset
                                                (merge-pathnames to-path root-path)
                                                (list (cons :line start) (cons :offset 0))))
                                            (:end-offset .
                                             ,(convert-to-top-offset
                                                (merge-pathnames to-path root-path)
                                                (list (cons :line end) (cons :offset -1)))))
                                          ranges))))))))
    (return-from diff-to-ranges (coerce ranges 'list))))

