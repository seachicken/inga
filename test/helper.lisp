(defpackage #:inga/test/helper
  (:use #:cl
        #:fiveam)
  (:import-from #:inga/ast-index
                #:ast-index-root-path
                #:ast-index-paths
                #:clean-indexes
                #:create-indexes
                #:get-ast)
  (:import-from #:inga/traversal
                #:traversal-java 
                #:ast-value
                #:convert-to-top-offset
                #:start-traversal
                #:stop-traversal)
  (:export #:*index*
           #:jvm-context
           #:node-context
           #:find-ast
           #:create-range))
(in-package #:inga/test/helper)

(def-fixture jvm-context (root-path index-type &key (include '("**")))
  (defparameter *root-path* root-path)
  (defparameter *index* nil)
  (defparameter *analyzers* nil)
  (defparameter *key-offset* "pos")
  (inga/plugin/jvm-dependency-loader:start root-path)
  (inga/plugin/spring-property-loader:start root-path)
  (setf *index* (make-instance index-type
                               :root-path root-path))
  (setf *analyzers*
        (list
          (start-traversal :java include nil root-path *index*)
          (start-traversal :kotlin include nil root-path *index*)))
  (&body)
  (loop for a in *analyzers* do (stop-traversal a))
  (inga/plugin/spring-property-loader:stop)
  (inga/plugin/jvm-dependency-loader:stop))

(def-fixture node-context (root-path index-type &key (include '("**")))
  (defparameter *root-path* root-path)
  (defparameter *index* nil)
  (defparameter *analyzers* nil)
  (setf *index* (make-instance index-type
                               :root-path root-path))
  (setf *analyzers*
        (list
          (start-traversal :typescript include nil root-path *index*)))
  (&body)
  (loop for a in *analyzers* do (stop-traversal a)))

(defun find-ast (path pos ast-index &key (key-offset *key-offset*))
  (loop with ast = (get-ast ast-index path)
        with offset = (convert-to-top-offset
                        (merge-pathnames path (ast-index-root-path ast-index)) pos)
        with stack = (list ast)
        do
        (setf ast (pop stack))
        (when (or
                (null ast)
                (eq (ast-value ast key-offset) offset))
          (return ast))

        (loop for child in (jsown:val ast "children")
              do (setf stack (append stack (list child))))))

(defun create-range (path &key line (start line) (end start) (root-path *root-path*))
  `((:path . ,path)
    (:start-offset .
     ,(convert-to-top-offset (merge-pathnames path root-path)
                             `((:line . ,start) (:offset . 0))))
    (:end-offset .
     ,(convert-to-top-offset (merge-pathnames path root-path)
                             `((:line . ,end) (:offset . -1))))))

