(defpackage #:inga/plugin/jvm-dependency-loader
  (:use #:cl
        #:inga/utils)
  (:import-from #:jsown)
  (:export #:start
           #:stop
           #:read-method))
(in-package #:inga/plugin/jvm-dependency-loader)

(defvar *jvm-dependency-loader*)
(defvar *root-path*)

(defun start (root-path)
  (setf *jvm-dependency-loader*
        (uiop:launch-program
          (format nil "~{~a~^ ~}"
                  `("java" "-cp"
                    ,(format nil "~a/libs/jvm-dependency-loader.jar" (uiop:getenv "INGA_HOME"))
                    "inga.jvmdependencyloader.Main"))
          :input :stream :output :stream))
  (setf *root-path* root-path)
  *jvm-dependency-loader*)

(defun stop ()
  (uiop:close-streams *jvm-dependency-loader*))

(defun read-method (fq-name from)
  (when (or (null fq-name) (equal fq-name ""))
    (return-from read-method))
  (let ((fqcn (format nil "~{~a~^.~}"
                      (butlast (split #\.
                                      (if (>= (length (split #\- fq-name)) 2)
                                          (first (butlast (split #\- fq-name)))
                                          fq-name))))))
    (loop for method in (jsown:parse
                          (exec-command *jvm-dependency-loader*
                                        (format nil "{\"fqcn\":\"~a\",\"from\":\"~a\"}"
                                                fqcn
                                                (merge-pathnames from *root-path*))))
          with target-name = (subseq (ppcre:regex-replace-all fqcn fq-name "") 1)
          with matched-methods
          do
          (let ((name (format nil "~a~:[~;-~]~:*~{~a~^-~}"
                              (jsown:val method "name")
                              (mapcar (lambda (type) (jsown:val type "name"))
                                      (jsown:val method "parameterTypes")))))
            (when (equal name target-name)
              (push method matched-methods)))
          finally
          (return (if (> (length matched-methods) 1)
                      (loop for method in matched-methods
                            do (unless (jsown:val (jsown:val method "returnType") "isInterface")
                                 (return method)))
                      (first matched-methods))))))

(defun exec-command (process cmd)
  (inga/utils::funtime
    (lambda ()
      (write-line cmd (uiop:process-info-input process))
      (force-output (uiop:process-info-input process))
      (read-line (uiop:process-info-output process)))
    :label "jvm-dependency-loader"
    :args cmd))

