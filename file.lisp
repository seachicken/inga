(defpackage #:inga/file
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:export #:is-match
           #:is-analysis-target))
(in-package #:inga/file)

(defun is-match (path candidates)
  (loop for candidate in candidates do
        (when (ppcre:scan (to-scan-str candidate) path)
          (return t))))

(defun is-analysis-target (path include &optional exclude)
  (unless (loop for inc in include do
                (when (ppcre:scan (to-scan-str inc) path)
                  (return t)))
    (return-from is-analysis-target))

  (loop for exc in exclude do
        (when (ppcre:scan (to-scan-str exc) path)
          (return-from is-analysis-target)))
  t)

(defun to-scan-str (str)
  (format nil "^~a$"
          (ppcre:regex-replace-all "\\*" str ".+")))

