(defpackage :inga/git
  (:use :cl)
  (:import-from :cl-ppcre)
  (:export #:get-diff))
(in-package :inga/git)

(defun get-diff (sha-a &optional (sha-b))
  (defparameter src-path "/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/test/fixtures/react-typescript-todo")
  (let ((diff (uiop:run-program (format nil "(cd ~a && git diff ~a ~a --unified=0)" src-path sha-a sha-b)
                                :output :string)))
    ;;(format t "d=~a~%" diff)

    (let ((ranges (make-array 10 :fill-pointer 0 :adjustable t)) to-path)
      (with-input-from-string (in diff)
        (loop :for line := (read-line in nil nil) :while line
              :do (progn
                    ;;(format t "line=~a~%" line)
                    (let ((found-to-path (car (cdr (multiple-value-list
                                                     (ppcre:scan-to-strings "^diff --git a/.+ b/(.+)$" line))))))
                      ;;(format t "ftp=~a~%" found-to-path)
                      (when found-to-path
                        (setq to-path (aref found-to-path 0))))
            
                    (let ((to-range (car (cdr (multiple-value-list
                                         (ppcre:scan-to-strings "@@.+\\+([0-9]+),?([0-9]*) @@" line))))))
                      ;;(format t "tr=~a~%" to-range)
                      (when to-range
                        (let ((start (parse-integer (aref to-range 0)))
                              (rows (if (> (length (aref to-range 1)) 0) (parse-integer (aref to-range 1)) 0)))
                          (let ((end (if (= rows 0) start (- (+ start rows) 1))))
                            (vector-push-extend (list (cons "path" to-path) (cons "start" start) (cons "end" end)) ranges)))))))
        (return-from get-diff (map 'list #'identity ranges))))))

