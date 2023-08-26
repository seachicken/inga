(defpackage #:inga/plugin/spring-property-loader
  (:use #:cl)
  (:import-from #:jsown)
  (:export #:start
           #:stop
           #:find-property))
(in-package #:inga/plugin/spring-property-loader)

(defun start ()
  (uiop:launch-program
    (format nil "~{~a~^ ~}"
            `("java" "-cp"
              ,(format nil "~a/libs/spring-property-loader.jar" (uiop:getenv "INGA_HOME"))
              "inga.springpropertyloader.Main"))
    :input :stream :output :stream))

(defun stop (process)
  (uiop:close-streams process))

(defun find-property (process key path)
  (let ((prod-profile-candidates '("production" "prod" "release")))
    (loop for property in (jsown:parse (exec-command process path))
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

(defun exec-command (process path)
  (write-line path (uiop:process-info-input process))
  (force-output (uiop:process-info-input process))
  (read-line (uiop:process-info-output process)))

