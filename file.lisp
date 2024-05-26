(defpackage #:inga/file
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:export #:is-match
           #:is-analysis-target
           #:get-file-type))
(in-package #:inga/file)

(defparameter *include-typescript*
  '("*.(js|jsx)"
    "*.(ts|tsx)"))
(defparameter *include-java*
  '("*.java"
    "*.kt"))

(defun is-match (path ctx-kind)
  (loop for inc in (get-language-files ctx-kind)
        do (when (ppcre:scan (to-scan-str inc) path)
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
      ((ppcre:scan (to-scan-str "*.java") path)
       :java)
      ((ppcre:scan (to-scan-str "*.kt") path)
       :kotlin)
      ((ppcre:scan (to-scan-str "*.(js|jsx|ts|tsx)") path)
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

