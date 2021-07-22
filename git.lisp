(defpackage :inga/git
  (:use :cl)
  (:import-from :cl-ppcre)
  (:export #:get-diff))
(in-package :inga/git)

(defun get-diff (sha-a &optional (sha-b))
  (defparameter src-path "/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/test/fixtures/react-typescript-todo")
  (let ((diff (uiop:run-program (format nil "(cd ~a && git diff ~a ~a --unified=0)" src-path sha-a sha-b)
                                :output :string)))
    (format t "d=~a~%" diff)
    (let ((ranges '()))
      (ppcre:do-matches-as-strings (match "@@.+@@" diff)
        (progn
          (format t "match=~a~%" match)
          (let ((to-range (car (cdr (multiple-value-list
                                       (ppcre:scan-to-strings "@@.+\\+([0-9]+),?([0-9]*) @@" match))))))
            (let ((start (parse-integer (aref to-range 0)))
                  (rows (if (> (length (aref to-range 1)) 0) (parse-integer (aref to-range 1)) 0)))
              (let ((end (if (= rows 0) start (- (+ start rows) 1))))
                (setq ranges (append ranges (list (cons "start" start) (cons "end" end)))))))))
      (return-from get-diff ranges))))

