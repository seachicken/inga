(defpackage #:inga/plugin/jvm-dependency-loader
  (:use #:cl
        #:inga/utils)
  (:import-from #:jsown)
  (:import-from #:inga/errors
                #:inga-error-process-not-running)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path)
  (:export #:start
           #:stop
           #:load-hierarchy
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
  (unless (uiop:process-alive-p *jvm-dependency-loader*)
    (error 'inga-error-process-not-running))

  (let ((base-path (find-base-path (merge-pathnames from *root-path*))))
    (unless base-path
      (return-from load-signatures))

    (jsown:parse
      (exec-command *jvm-dependency-loader*
                    (format nil "{\"type\":\"METHODS\",\"fqcn\":\"~a\",\"from\":\"~a\"}"
                            fq-class-name
                            base-path)))))

(defun load-hierarchy (fq-class-name from)
  (unless (uiop:process-alive-p *jvm-dependency-loader*)
    (error 'inga-error-process-not-running))

  (let ((base-path (find-base-path (merge-pathnames from *root-path*))))
    (unless base-path
      (return-from load-hierarchy))

    (mapcar (lambda (h)
              (jsown:val h "name"))
            (jsown:parse
              (exec-command *jvm-dependency-loader*
                            (format nil "{\"type\":\"HIERARCHY\",\"fqcn\":\"~a\",\"from\":\"~a\"}"
                                    fq-class-name
                                    base-path))))))

(defun exec-command (process cmd)
  (inga/utils::funtime
    (lambda ()
      (write-line cmd (uiop:process-info-input process))
      (force-output (uiop:process-info-input process))
      (read-line (uiop:process-info-output process)))
    :label "jvm-dependency-loader"
    :args cmd))

