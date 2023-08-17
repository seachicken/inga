(defpackage #:inga/plugin/jvm-dependency-loader
  (:use #:cl
        #:inga/utils)
  (:import-from #:jsown)
  (:export #:start
           #:stop
           #:load-project
           #:read-method))
(in-package #:inga/plugin/jvm-dependency-loader)

(defun start ()
  (uiop:launch-program
    (format nil "~{~a~^ ~}"
            `("java" "-cp"
              ,(format nil "~a/libs/jvm-dependency-loader.jar" (uiop:getenv "INGA_HOME"))
              "inga.jvmdependencyloader.Main"))
    :input :stream :output :stream))

(defun stop (process)
  (uiop:close-streams process))

(defun load-project (process path)
  (exec-command
    process
    (format nil "{\"command\":\"load\",\"path\":\"~a\"}" path)))

(defun read-method (process fqn)
  (let ((fqcn (format nil "~{~a~^.~}"
                      (butlast (split #\. (first (butlast (split #\- fqn))))))))
    (loop for method in (jsown:parse
                          (exec-command
                            process
                            (format nil "{\"command\":\"read\",\"path\":\"~a\"}" fqcn)))
          with target-name = (subseq (ppcre:regex-replace-all fqcn fqn "") 1)
          do
          (let ((name (format nil "~a~:[~;-~]~:*~{~a~^-~}"
                              (jsown:val method "name")
                              (jsown:val method "parameterTypes"))))
            (when (equal name target-name)
              (return method))))))

(defun exec-command (process path)
  (write-line path (uiop:process-info-input process))
  (force-output (uiop:process-info-input process))
  (read-line (uiop:process-info-output process)))

