(defpackage #:inga/file
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:export #:is-match
           #:is-analysis-target
           #:get-file-type
           #:convert-to-top-offset
           #:convert-to-pos))
(in-package #:inga/file)

(defparameter *include-typescript*
  '(".js"
    ".jsx"
    ".ts"
    ".tsx"))
(defparameter *include-java*
  '(".java"
    ".kt"))

(defun is-match (path ctx-kind)
  (loop for inc in (get-language-files ctx-kind)
        do (when (uiop:string-suffix-p path inc)
             (return t))))

(defun is-analysis-target (ctx-kind path &optional include exclude)
  (unless (is-match path ctx-kind)
    (return-from is-analysis-target))
  (and include (unless (loop for inc in include do
                             (when (ppcre:scan (to-scan-str inc) path)
                               (return t)))
                 (return-from is-analysis-target)))
  (loop for exc in exclude do
        (when (ppcre:scan (to-scan-str exc) path)
          (return-from is-analysis-target)))
  t)

(defun get-file-type (path)
  (let ((path (namestring path)))
    (cond
      ((uiop:string-suffix-p path ".java")
       :java)
      ((uiop:string-suffix-p path ".kt")
       :kotlin)
      ((is-match path :typescript)
       :typescript))))

(defun get-language-files (kind)
  (cond
    ((eq kind :java)
     *include-java*)
    ((eq kind :typescript)
     *include-typescript*)))

(defun to-scan-str (str)
  (format nil "^~a$"
          (ppcre:regex-replace-all
            "\\*"
            ;; replace from "." with escaped dot "\."
            (ppcre:regex-replace-all "\\." str "\\.")
            ".+")))

(defun convert-to-top-offset (path pos)
  (with-open-file (stream path)
    (loop for file-line = (read-line stream nil)
          with line-no = 0
          with top-offset = 0
          while file-line
          when (eq line-no (1- (cdr (assoc :line pos))))
          return (+ top-offset (if (< (cdr (assoc :offset pos)) 0)
                                   (length file-line)
                                   (1- (cdr (assoc :offset pos)))))
          do
          (setf line-no (1+ line-no))
          ;; add newline code
          (setq top-offset (+ top-offset (length file-line) 1)))))

(defun convert-to-pos (path top-offset)
  (with-open-file (stream path)
    (loop for file-line = (read-line stream nil)
          with line-no = 0
          with current-offset = 0
          while file-line
          when (<= top-offset (+ current-offset (length file-line)))
          return (list
                   (cons :line (1+ line-no))
                   (cons :offset (- (1+ (length file-line))
                                    (- (+ current-offset (length file-line)) top-offset))))
          do
          (setf line-no (1+ line-no))
          ;; add newline code
          (setf current-offset (+ current-offset (length file-line) 1)))))

