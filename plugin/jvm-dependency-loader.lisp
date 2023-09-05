(defpackage #:inga/plugin/jvm-dependency-loader
  (:use #:cl
        #:inga/utils)
  (:import-from #:jsown)
  (:export #:start
           #:stop
           #:load-signatures))
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

(defun load-signatures (fq-class-name from)
  (jsown:parse
    (exec-command *jvm-dependency-loader*
                  (format nil "{\"fqcn\":\"~a\",\"from\":\"~a\"}"
                          fq-class-name
                          (merge-pathnames from *root-path*)))))

(defun exec-command (process cmd)
  (inga/utils::funtime
    (lambda ()
      (write-line cmd (uiop:process-info-input process))
      (force-output (uiop:process-info-input process))
      (read-line (uiop:process-info-output process)))
    :label "jvm-dependency-loader"
    :args cmd))

