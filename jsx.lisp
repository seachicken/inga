(defpackage #:inga/jsx
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:export #:inject-mark-on-line))
(in-package #:inga/jsx)

(defun inject-mark-on-line (line-string offset score)
  (if (and (and (> offset 1) (< offset (length line-string)))
           (equal (subseq line-string (- offset 2) (- offset 1)) "<"))
      (let ((tag-end-offset (get-tag-end-offset line-string offset)))
        (format nil "~a data-inga=\"~a\"~a"
                (subseq line-string 0 tag-end-offset)
                score
                (subseq line-string tag-end-offset)))
      (progn
        (format *error-output* "Can't inject mark. line-string: \"~a\", offset: ~a~%" line-string offset)
        line-string)))

(defun get-tag-end-offset (line-string offset)
  (+ offset
     (length (first (ppcre:split
                      " "
                      (subseq line-string offset))))))
