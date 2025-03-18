(defpackage #:inga/ast-parser/java
  (:use #:cl
        #:inga/ast-parser/base
        #:inga/utils)
  (:import-from #:inga/logger
                #:log-error)
  (:import-from #:inga/plugin/jvm-dependency-loader
                #:load-class-paths)
  (:export #:ast-parser-java))
(in-package #:inga/ast-parser/java)

(defclass ast-parser-java (ast-parser)
  ())

(defmethod start ((file-type (eql :java)))
  (make-instance
    'ast-parser-java
    :process (uiop:launch-program
               (format nil "~{~a~^ ~}"
                       `("java" "-cp"
                         ,(format nil "~a/libs/javaparser.jar" (uiop:getenv "INGA_HOME"))
                         "--add-opens" "jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED"
                         "--add-opens" "jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED"
                         "--add-opens" "jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED"
                         "inga.Main"))
               :input :stream :output :stream :error-output :stream)))

(defmethod stop ((ast-parser ast-parser-java))
  (uiop:close-streams (ast-parser-process ast-parser)))

(defmethod parse-generic ((ast-parser ast-parser-java) path)
  (let ((command (format nil "~a --analyze ~{~a~^:~}"
                         (namestring path)
                         (load-class-paths path))))
    (funtime
      (lambda ()
        (write-line command (uiop:process-info-input (ast-parser-process ast-parser)))
        (force-output (uiop:process-info-input (ast-parser-process ast-parser)))
        (prog1
          (read-line (uiop:process-info-output (ast-parser-process ast-parser)) nil nil)
          (loop while (listen (uiop:process-info-error-output (ast-parser-process ast-parser)))
                with results = ""
                do (setf results
                         (format nil "~a~a~%"
                                 results
                                 (read-line (uiop:process-info-error-output
                                              (ast-parser-process ast-parser)))))
                finally (unless (equal results "")
                          (log-error (format nil "~a, cmd: ~a" results command))))))
      :label "javaparser"
      :args command)))

(defmethod version-generic ((ast-parser ast-parser-java))
  (uiop:getenv "JAVAPARSER"))

