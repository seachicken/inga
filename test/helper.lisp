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
  (defparameter *key-offset* "pos")
  (inga/plugin/jvm-dependency-loader:start root-path)
  (inga/plugin/spring-property-loader:start root-path)
  (setf *index* (make-instance index-type
                               :root-path root-path))
  (let ((java (start-traversal :java include nil root-path *index*))
        (kotlin (start-traversal :kotlin include nil root-path *index*)))
    (unwind-protect
      (&body)
      (progn
        (stop-traversal java)
        (stop-traversal kotlin)
        (inga/plugin/spring-property-loader:stop)
        (inga/plugin/jvm-dependency-loader:stop)))))

(def-fixture node-context (root-path index-type &key (include '("**")))
  (defparameter *root-path* root-path)
  (defparameter *index* nil)
  (setf *index* (make-instance index-type
                               :root-path root-path))
  (let ((typescript (start-traversal :typescript include nil root-path *index*)))
    (unwind-protect
      (&body)
      (stop-traversal typescript))))

(defmacro find-ast (path pos &key key-offset)
  `(let ((result
           (loop
             with offset = (convert-to-top-offset
                             (merge-pathnames ,path (ast-index-root-path *index*))
                             ,pos)
             with stack = (list (get-ast *index* ,path))
             with ast 
             do
             (setf ast (pop stack))
             (when (or (null ast)
                       (eq (ast-value ast (or ,key-offset *key-offset*)) offset))
               (return ast))
             (loop for child in (ast-value ast "children")
                   do (setf stack (append stack (list child)))))))
     (if result
         result
         (error "ast not found"))))

(defmacro create-range (path &key line (start line) (end start))
  `(list
     (cons :path ,path)
     (cons :start-offset
           (convert-to-top-offset (merge-pathnames ,path *root-path*)
                                  (list (cons :line ,start) (cons :offset 0))))
     (cons :end-offset
           (convert-to-top-offset (merge-pathnames ,path *root-path*)
                                  (list (cons :line ,end) (cons :offset -1))))))

