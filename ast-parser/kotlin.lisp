(defpackage #:inga/ast-parser/kotlin
  (:use #:cl
        #:inga/ast-parser/base)
  (:export #:ast-parser-kotlin))
(in-package #:inga/ast-parser/kotlin)

(defclass ast-parser-kotlin (ast-parser)
  ())

(defmethod start ((file-type (eql :kotlin)))
  (make-instance
    'ast-parser-kotlin
    :process (uiop:launch-program
               (format nil "~{~a~^ ~}"
                       `("java" "-jar"
                         ,(format nil "~a/libs/ktparser.jar" (uiop:getenv "INGA_HOME"))))
               :input :stream :output :stream :error-output :stream)))

(defmethod stop ((ast-parser ast-parser-kotlin))
  (uiop:close-streams (ast-parser-process ast-parser)))

(defmethod version-generic ((ast-parser ast-parser-kotlin))
  (uiop:getenv "KTPARSRER"))

