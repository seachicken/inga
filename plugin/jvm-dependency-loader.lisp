(defpackage #:inga/plugin/jvm-dependency-loader
  (:use #:cl
        #:inga/utils)
  (:import-from #:jsown)
  (:import-from #:inga/errors
                #:inga-error-process-not-running
                #:inga-error-process-failed)
  (:import-from #:inga/logger
                #:log-error)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path
                #:is-primitive-type)
  (:export #:start
           #:stop
           #:load-class-paths
           #:load-hierarchy
           #:load-structure
           #:load-signatures))
(in-package #:inga/plugin/jvm-dependency-loader)

(defparameter *jvm-dependency-loader* nil)
(defparameter *root-path* nil)

(defun start (root-path)
  (setf *jvm-dependency-loader*
        (uiop:launch-program
          (format nil "~{~a~^ ~}"
                  `("java" "-jar"
                    ,(format nil "~a/libs/jvm-dependency-loader.jar" (uiop:getenv "INGA_HOME"))))
          :input :stream :output :stream :error-output :stream))
  (setf *root-path* root-path)
  *jvm-dependency-loader*)

(defun stop ()
  (uiop:close-streams *jvm-dependency-loader*))

(defun load-class-paths (from)
  (unless (uiop:process-alive-p *jvm-dependency-loader*)
    (error 'inga-error-process-not-running))
  
  (let ((base-path (find-base-path from *root-path*)))
    (unless base-path
      (return-from load-class-paths))
    
    (let ((results (exec-command
                     *jvm-dependency-loader*
                     (format nil "{\"type\":\"CLASS_PATHS\",\"from\":\"~a\",\"root\":\"~a\"}"
                             base-path *root-path*))))
      (when results (jsown:parse results)))))

(defun load-signatures (fq-class-name from)
  (unless (uiop:process-alive-p *jvm-dependency-loader*)
    (error 'inga-error-process-not-running))
  (when (or (null fq-class-name) (equal fq-class-name "")
            ;; TODO: remove NIL check when correctly got fq-class-name
            (equal fq-class-name "NIL"))
    (return-from load-signatures))

  (let ((base-path (find-base-path from *root-path*)))
    (unless base-path
      (return-from load-signatures))

    (when (uiop:string-suffix-p fq-class-name "[]")
      (setf fq-class-name (string-right-trim "[]" fq-class-name)))

    (let ((results (exec-command
                     *jvm-dependency-loader*
                     (format nil "{\"type\":\"METHODS\",\"fqcn\":\"~a\",\"from\":\"~a\",\"root\":\"~a\"}"
                             fq-class-name base-path *root-path*))))
      (when results
        (mapcar (lambda (method)
                  (let* ((return-type (when (jsown:keyp method "returnType")
                                        (jsown:val method "returnType")))
                         (return-name (when (jsown:keyp return-type "name")
                                 (jsown:val return-type "name"))))
                    (when (find-if (lambda (name) (equal name return-name))
                                   '("boolean"
                                     "long"
                                     "short"
                                     "int"
                                     "long"
                                     "char"
                                     "float"
                                     "double"))
                      (setf (jsown:val return-type "name") (string-upcase return-name)))
                    `((:name . ,(first (last (split #\. (jsown:val method "name")))))
                      (:fq-name .
                       ,(format nil "~a~:[~;-~]~:*~{~a~^-~}"
                                (jsown:val method "name")
                                (mapcar (lambda (type)
                                          (if (jsown:val type "isArray")
                                              (concatenate 'string (jsown:val type "name") "[]")
                                              (jsown:val type "name")))
                                        (jsown:val method "parameterTypes"))))
                      (:return . ,(jsown:val return-type "name")))))
                (jsown:parse results))))))

(defun load-structure (fq-class-name from)
  (unless (uiop:process-alive-p *jvm-dependency-loader*)
    (error 'inga-error-process-not-running))
  (when (or (null fq-class-name) (equal fq-class-name "")
            ;; TODO: remove NIL check when correctly got fq-class-name
            (equal fq-class-name "NIL")
            (null from))
    (return-from load-structure))

  (let ((base-path (find-base-path from *root-path*)))
    (unless base-path
      (return-from load-structure))
    
    (when (uiop:string-suffix-p fq-class-name "[]")
      (setf fq-class-name (string-right-trim "[]" fq-class-name)))

    (let ((results (exec-command
                     *jvm-dependency-loader*
                     (format nil "{\"type\":\"CLASSES\",\"fqcn\":\"~a\",\"from\":\"~a\",\"root\":\"~a\"}"
                             fq-class-name base-path *root-path*))))
      (when results (jsown:parse results)))))

(defun load-hierarchy (fq-class-name from)
  (unless (uiop:process-alive-p *jvm-dependency-loader*)
    (error 'inga-error-process-not-running))
  (when (is-primitive-type fq-class-name)
    (return-from load-hierarchy))
  (when (or (null fq-class-name) (equal fq-class-name "")
            ;; TODO: remove NIL check when correctly got fq-class-name
            (equal fq-class-name "NIL"))
    (return-from load-hierarchy))

  (let ((base-path (find-base-path from *root-path*)))
    (unless base-path
      (return-from load-hierarchy))

    (when (uiop:string-suffix-p fq-class-name "[]")
      (setf fq-class-name (string-right-trim "[]" fq-class-name)))

    (mapcar (lambda (h) (jsown:val h "name"))
            (let ((results (exec-command
                             *jvm-dependency-loader*
                             (format nil "{\"type\":\"HIERARCHY\",\"fqcn\":\"~a\",\"from\":\"~a\",\"root\":\"~a\"}"
                                     fq-class-name base-path *root-path*))))
              (when results (jsown:parse results))))))

(defparameter *command-lock* (sb-thread:make-mutex))

(defun exec-command (process cmd)
  (funtime
    (lambda ()
      (sb-thread:with-mutex (*command-lock*)
        (handler-case
          (progn
            (write-line cmd (uiop:process-info-input process))
            (force-output (uiop:process-info-input process))
            (prog1
              (read-line (uiop:process-info-output process) nil nil)
              (loop while (listen (uiop:process-info-error-output process))
                    with results = ""
                    do (setf results
                             (format nil "~a~a~%"
                                     results
                                     (read-line (uiop:process-info-error-output process))))
                    finally (unless (equal results "")
                              (log-error (format nil "~a, cmd: ~a" results cmd))))))
          (error () (error 'inga-error-process-failed)))))
    :label "jvm-dependency-loader"
    :args cmd))

