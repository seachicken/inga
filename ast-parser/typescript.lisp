(defpackage #:inga/ast-parser/typescript
  (:use #:cl
        #:inga/ast-parser/base)
  (:export #:ast-parser-typescript))
(in-package #:inga/ast-parser/typescript)

(defclass ast-parser-typescript (ast-parser)
  ())

(defmethod start ((file-type (eql :typescript)))
  (make-instance
    'ast-parser-typescript
    :process (uiop:launch-program
               "tsparser"
               :input :stream :output :stream :error-output :stream)))

(defmethod stop ((ast-parser ast-parser-typescript))
  (uiop:close-streams (ast-parser-process ast-parser)))

(defmethod version-generic ((ast-parser ast-parser-typescript))
  (uiop:getenv "TSPARSRER"))

