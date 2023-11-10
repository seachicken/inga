(defpackage #:inga/test/helper
  (:use #:cl
        #:fiveam)
  (:import-from #:inga/ast-index
                #:ast-index-root-path
                #:clean-indexes
                #:create-indexes
                #:get-ast)
  (:import-from #:inga/ast-analyzer
                #:ast-analyzer-java 
                #:ast-value
                #:convert-to-top-offset
                #:create-index-groups
                #:get-index-path
                #:start-ast-analyzer
                #:stop-ast-analyzer)
  (:export #:*index*
           #:jvm-context
           #:node-context
           #:find-ast))
(in-package #:inga/test/helper)

(def-fixture jvm-context (root-path index-type)
  (defparameter *index* nil)
  (defparameter *analyzers* nil)
  (inga/plugin/jvm-dependency-loader:start root-path)
  (inga/plugin/spring-property-loader:start root-path)
  (setf *index* (make-instance index-type
                               :root-path root-path))
  (create-indexes *index* inga/main::*include-java* nil)
  (create-index-groups *index*)
  (setf *analyzers*
        (list
          (start-ast-analyzer :java nil root-path *index*)
          (start-ast-analyzer :kotlin nil root-path *index*)))
  (&body)
  (loop for a in *analyzers* do (stop-ast-analyzer a))
  (clean-indexes *index*)
  (inga/plugin/spring-property-loader:stop)
  (inga/plugin/jvm-dependency-loader:stop))

(def-fixture node-context (root-path index-type)
  (defparameter *index* nil)
  (defparameter *analyzers* nil)
  (setf *index* (make-instance index-type
                               :root-path root-path))
  (create-indexes *index* inga/main::*include-typescript* nil)
  (setf *analyzers*
        (list
          (start-ast-analyzer :typescript nil root-path)))
  (&body)
  (loop for a in *analyzers* do (stop-ast-analyzer a))
  (clean-indexes *index*))

(defun find-ast (path pos ast-index)
  (loop with ast = (get-ast ast-index path)
        with offset = (convert-to-top-offset (merge-pathnames path (ast-index-root-path ast-index)) pos)
        with stack = (list ast)
        do
        (setf ast (pop stack))
        (when (or
                (null ast)
                (eq (ast-value ast "pos") offset))
          (return ast))

        (loop for child in (jsown:val ast "children")
              do (setf stack (append stack (list child))))))
