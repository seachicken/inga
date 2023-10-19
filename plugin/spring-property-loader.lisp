(defpackage #:inga/plugin/spring-property-loader
  (:use #:cl)
  (:import-from #:jsown)
  (:import-from #:inga/cache
                #:defunc)
  (:import-from #:inga/errors
                #:inga-error-process-not-running
                #:inga-error-process-failed) 
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path)
  (:export #:start
           #:stop
           #:find-property))
(in-package #:inga/plugin/spring-property-loader)

(defvar *spring-property-loader*)
(defvar *root-path*)

(defun start (root-path)
  (setf *spring-property-loader*
        (uiop:launch-program
          (format nil "~{~a~^ ~}"
                  `("java" "-cp"
                    ,(format nil "~a/libs/spring-property-loader.jar" (uiop:getenv "INGA_HOME"))
                    "inga.springpropertyloader.Main"))
          :input :stream :output :stream :error-output :stream))
  (setf *root-path* root-path)
  *spring-property-loader*)

(defun stop ()
  (uiop:close-streams *spring-property-loader*))

(defun find-property (key from)
  (unless (uiop:process-alive-p *spring-property-loader*)
    (error 'inga-error-process-not-running))

  (let ((prod-profile-candidates '("production" "prod" "release"))
        (base-path (find-base-path (merge-pathnames from *root-path*))))
    (unless base-path
      (return-from find-property))

    (loop for property in (jsown:parse
                            (exec-command *spring-property-loader* (namestring base-path)))
          with result
          do
          (when (and
                  (jsown:keyp property key)
                  (or (not (jsown:keyp property "spring.config.activate.on-profile"))
                      (find (jsown:val property "spring.config.activate.on-profile")
                            prod-profile-candidates
                            :test #'equal)))
            (setf result (jsown:val property key)))
          finally (return (if (and (equal key "server.port") (null result))
                              "8080"
                              result)))))

(defunc exec-command (process cmd)
  (inga/utils::funtime
    (lambda ()
      (handler-case
        (progn
          (write-line cmd (uiop:process-info-input process))
          (force-output (uiop:process-info-input process))
          (read-line (uiop:process-info-output process)))
        (error (e)
               (princ (uiop:slurp-stream-string (uiop:process-info-error-output process)))
               (error 'inga-error-process-failed))))
    :label "spring-property-loader"
    :args cmd))

