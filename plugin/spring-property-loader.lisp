(defpackage #:inga/plugin/spring-property-loader
  (:use #:cl)
  (:import-from #:jsown)
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
          :input :stream :output :stream))
  (setf *root-path* root-path)
  *spring-property-loader*)

(defun stop ()
  (uiop:close-streams *spring-property-loader*))

(defun find-property (key from)
  (let ((prod-profile-candidates '("production" "prod" "release")))
    (loop for property in (jsown:parse
                            (exec-command *spring-property-loader*
                                          (namestring (merge-pathnames from *root-path*))))
          with result
          do
          (when (and
                  (jsown:keyp property key)
                  (or (not (jsown:keyp property "spring.config.activate.on-profile"))
                      (find (jsown:val property "spring.config.activate.on-profile")
                            prod-profile-candidates
                            :test #'equal)))
            (setf result (jsown:val property key)))
          finally (return result))))

(defun exec-command (process cmd)
  (inga/utils::funtime
    (lambda ()
      (write-line cmd (uiop:process-info-input process))
      (force-output (uiop:process-info-input process))
      (read-line (uiop:process-info-output process)))
    :label "spring-property-loader"
    :args cmd))

