(defpackage #:inga/plugin/spring-property-loader
  (:use #:cl)
  (:import-from #:jsown)
  (:import-from #:inga/cache
                #:defunc)
  (:import-from #:inga/errors
                #:inga-error-process-not-running
                #:inga-error-process-failed) 
  (:import-from #:inga/logger
                #:log-error)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path)
  (:export #:start
           #:stop
           #:find-property))
(in-package #:inga/plugin/spring-property-loader)

(defparameter *spring-property-loader* nil)
(defparameter *root-path* nil)

(defun start (root-path)
  (setf *spring-property-loader*
        (uiop:launch-program
          (format nil "~{~a~^ ~}"
                  `("java" "-jar"
                    ,(format nil "~a/libs/spring-property-loader.jar" (uiop:getenv "INGA_HOME"))))
          :input :stream :output :stream :error-output :stream))
  (setf *root-path* root-path)
  *spring-property-loader*)

(defun stop ()
  (uiop:close-streams *spring-property-loader*))

(defun find-property (key from)
  (unless (uiop:process-alive-p *spring-property-loader*)
    (error 'inga-error-process-not-running))

  (let ((base-path (find-base-path (merge-pathnames from *root-path*))))
    (unless base-path
      (return-from find-property))

    (let ((results (exec-command
                     *spring-property-loader*
                     (format nil "{\"from\":\"~a\",\"profileCandidates\":[\"production\",\"prod\",\"release\"]}"
                             (namestring base-path)))))
      (unless results (return-from find-property))

      (loop for property in (jsown:parse results)
            with result
            do
            (when (jsown:keyp property key)
              (setf result (jsown:val property key)))
            finally (return (if (and (equal key "server.port") (null result))
                                "8080"
                                result))))))

(defunc exec-command (process cmd)
  (inga/utils::funtime
    (lambda ()
      (handler-case
        (progn
          (write-line cmd (uiop:process-info-input process))
          (force-output (uiop:process-info-input process))
          (prog1
            (read-line (uiop:process-info-output process))  
            (loop while (listen (uiop:process-info-error-output process))
                  with results = ""
                  do (setf results
                           (format nil "~a~a~%"
                                   results 
                                   (read-line (uiop:process-info-error-output process))))
                  finally (unless (equal results "")
                            (log-error (format nil "~a, cmd: ~a" results cmd))))))
        (error () (error 'inga-error-process-failed))))
    :label "spring-property-loader"
    :args cmd))

