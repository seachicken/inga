(defpackage #:inga/git
  (:use #:cl
        #:inga/file)
  (:import-from #:cl-ppcre)
  (:export #:get-diff
           #:get-hostname
           #:track-branch))
(in-package #:inga/git)

(defun get-diff (project-path sha-a include &optional sha-b exclude)
  (let ((diff (uiop:run-program (if (null sha-b)
                                    (format nil "(cd ~a && git diff ~a --unified=0)" project-path sha-a)
                                    (format nil "(cd ~a && git diff ~a ~a --unified=0)" project-path sha-a sha-b))
                                :output :string)))

    (let ((ranges (make-array 10 :fill-pointer 0 :adjustable t)) to-path)
      (with-input-from-string (in diff)
        (loop :for line := (read-line in nil nil) :while line
              :do (progn
                    (let ((found-to-path (car (cdr (multiple-value-list
                                                     (ppcre:scan-to-strings "^diff --git a/.+ b/(.+)$" line))))))
                      (when found-to-path
                        (setq to-path (aref found-to-path 0))))
                    (when (is-analysis-target to-path include exclude)
                      (let ((to-range (car (cdr (multiple-value-list
                                                  (ppcre:scan-to-strings "@@.+\\+([0-9]+),?([0-9]*) @@" line))))))
                        (when to-range
                          (let ((start (parse-integer (aref to-range 0)))
                                (rows (if (> (length (aref to-range 1)) 0) (parse-integer (aref to-range 1)) 0)))
                            (let ((end (if (= rows 0) start (- (+ start rows) 1))))
                              ;; extract undeleted files
                              (when (or (> start 0) (> end 0))
                                (vector-push-extend (list (cons "path" to-path)
                                                          (cons "start" start)
                                                          (cons "end" end)) ranges))))))))))
      (return-from get-diff (coerce ranges 'list)))))

(defun get-hostname (project-path)
  (let ((result (uiop:run-program (format nil "(cd ~a && git remote -v)" project-path)
                                  :output :string
                                  :ignore-error-status t)))
    (aref
      (nth 1
           (multiple-value-list
             (ppcre:scan-to-strings
               "^(.+?)\\s+.+(?:@|//)(.+?)(?::|/)(.+?/.+?)(?:\.git|)\\s+.+$"
               result))) 1)))

(defun track-branch (ref-name project-path)
  (uiop:run-program (format nil
                            "(cd ~a && git branch --track ~a origin/~a)"
                            project-path
                            ref-name ref-name)
                    :ignore-error-status t))

