(defpackage #:inga/ast-parser/java
  (:use #:cl
        #:inga/ast-parser/base)
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

(defmethod version-generic ((ast-parser ast-parser-java))
  (uiop:getenv "JAVAPARSER"))

