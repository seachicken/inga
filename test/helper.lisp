(defpackage #:inga/test/helper
  (:use #:cl
        #:fiveam)
  (:import-from #:inga/ast-index
                #:ast-index-root-path
                #:ast-index-paths
                #:clean-indexes
                #:create-indexes
                #:get-ast)
  (:import-from #:inga/file
                #:get-file-type)
  (:import-from #:inga/traversal
                #:find-ast
                #:traversal-java 
                #:ast-value
                #:convert-to-top-offset
                #:start-traversal
                #:stop-traversal)
  (:export #:*index*
           #:jvm-context
           #:node-context
           #:ast
           #:create-range))
(in-package #:inga/test/helper)

(def-fixture jvm-context (root-path index-type &key (include '("**")))
  (defparameter *root-path* root-path)
  (defparameter *index* nil)
  (inga/plugin/jvm-dependency-loader:start root-path)
  (inga/plugin/spring/spring-property-loader:start root-path)
  (setf *index* (make-instance index-type
                               :root-path root-path))
  (let ((java (start-traversal :java include nil root-path *index*))
        (kotlin (start-traversal :kotlin include nil root-path *index*)))
    (unwind-protect
      (&body)
      (progn
        (stop-traversal java)
        (stop-traversal kotlin)
        (inga/plugin/spring/spring-property-loader:stop)
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

(defmacro ast (readable-pos)
  `(let* ((path (cdr (assoc :path ,readable-pos)))
          (pos (list (cons :path path)
                     (cons :top-offset
                           (convert-to-top-offset
                             (merge-pathnames path (ast-index-root-path *index*))
                             ,readable-pos))))
          (result (trav:find-ast pos *index*)))
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

