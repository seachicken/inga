(defpackage #:inga/main
  (:use #:cl
        #:inga/utils)
  (:import-from #:alexandria)
  (:import-from #:inga/contexts
                #:*mode*)
  (:import-from #:inga/file
                #:is-match)
  (:import-from #:inga/utils
                #:split-trim-comma)
  (:import-from #:inga/errors
                #:inga-error)
  (:import-from #:inga/git
                #:diff-to-ranges)
  (:import-from #:inga/logger
                #:log-debug
                #:log-error)
  (:import-from #:inga/yaml
                #:parse-yaml)
  (:export #:command))
(in-package #:inga/main)

(define-condition inga-error-option-not-found (inga-error) ())

(defun command (argv)
  (handler-case
    (let* ((params (parse-argv argv))
           (language (first (filter-active-context
                              (get-analysis-kinds (diff-to-ranges
                                                    (cdr (assoc :diff params))
                                                    (cdr (assoc :root-path params))))
                              (get-env-kinds))))
           (config-path (merge-pathnames ".inga.yml" (cdr (assoc :root-path params)))))
      (when (probe-file config-path)
        (setf params
              (acons :config (parse-yaml (alexandria:read-file-into-string config-path)) params)))
      (run (cdr (assoc :mode params)) language params))
    (inga-error (e) (format t "~a~%" e))))

(defun parse-argv (argv)
  (loop with root-path = "."
        with output-path
        with temp-path
        with include
        with exclude
        with diff = ""
        with mode = :cli
        for option = (pop argv)
        while option
        do (alexandria:switch (option :test #'equal)
             ("--diff"
              (setf diff (pop argv)))
             ("--root-path"
              (setf root-path (pop argv)))
             ("--front-path"
              (setf root-path (pop argv)))
             ("--back-path"
              (setf root-path (pop argv)))
             ("--output-path"
              (setf output-path (pop argv)))
             ("--temp-path"
              (setf temp-path (pop argv)))
             ("--include"
              (setf include (split-trim-comma (pop argv))))
             ("--exclude"
              (setf exclude (split-trim-comma (pop argv))))
             ("--base-commit"
              (log-error "base-commit option is deprecated")
              (pop argv))
             ("--mode"
              (setf mode (intern (string-upcase (pop argv)) :keyword)))
             (t (error 'inga-error-option-not-found)))
        finally
        (setf *mode* mode)
        (return
          (let ((result
                  `((:diff . ,(get-diff diff))
                    (:root-path . ,(truename (uiop:merge-pathnames* root-path)))
                    (:output-path .
                     ,(if output-path
                          (pathname (concatenate 'string output-path "/"))
                          (merge-pathnames ".inga/")))
                    (:temp-path .
                     ,(if temp-path
                          (pathname (concatenate 'string temp-path "/"))
                          (merge-pathnames ".inga/")))
                    (:include . ,include)
                    (:exclude . ,exclude)
                    (:mode . ,mode))))
            result))))

(defun get-diff (input)
  (if (equal input "-")
      (loop while (listen *standard-input*)
            with result = ""
            do (setf result
                     (format nil "~a~a~%"
                             result
                             (read-line *standard-input* nil)))
            finally (return result))
      input))

(defun get-analysis-kinds (ranges)
  (remove nil
          (remove-duplicates
            (mapcar (lambda (diff)
                      (if (is-match (cdr (assoc :path diff)) :typescript)
                          :typescript
                          (if (is-match (cdr (assoc :path diff)) :java)
                              :java
                              nil)))
                    ranges))))

(defun get-env-kinds ()
  (let ((kinds (split-trim-comma (uiop:getenv "INGA_CONTEXT"))))
    (remove nil
            (remove-duplicates
              (mapcar (lambda (kind)
                        (alexandria:switch (kind :test #'equal)
                          ("typescript" :typescript)
                          ("java" :java)))
                      kinds)))))

(defun filter-active-context (found-context env-context)
  (log-debug (format nil "found context: ~a, env context: ~a" found-context env-context))
  (if env-context
      (mapcar (lambda (kind)
                (when (find kind found-context) kind))
              env-context)
      found-context))

(defgeneric run (mode language params)
  (:method (mode language params)
   (error (format nil "unknown mode: ~a" mode))))

