(defpackage #:inga/git
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:import-from #:inga/errors
                #:inga-error)
  (:export #:get-diff
           #:is-ignore))
(in-package #:inga/git)

(defun get-diff (project-path base-commit)
  (let ((diff (uiop:run-program
                (format nil "(cd ~a && git diff ~a --unified=0 --)"
                        project-path
                        (if base-commit base-commit ""))
                :output :string)))
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
      (return-from get-diff (coerce ranges 'list)))))

(defun is-ignore (project-path relative-path)
  (multiple-value-bind (output error-output exit-code)
    (uiop:run-program
      (format nil "(cd ~a && git check-ignore ~a)" project-path relative-path)
      :ignore-error-status t)
    (eq exit-code 0)))

