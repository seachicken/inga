(defpackage :inga/jsx
  (:use :cl)
  (:import-from :cl-ppcre)
  (:export #:inject-mark-on-line))
(in-package :inga/jsx)

(defun inject-mark-on-line (line-string offset score)
  (if (equal (subseq line-string (- offset 2) (- offset 1)) "<")
      (progn
        (defparameter tag-end-offset
          (+ 
            offset
            (length (first (ppcre:split
                             " "
                             (subseq line-string offset))))))
        (format nil "~a data-inga=\"~a\"~a"
                (subseq line-string 0 tag-end-offset)
                score
                (subseq line-string tag-end-offset)))
      line-string))

