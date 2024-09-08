(defpackage #:inga/test/helper
  (:use #:cl
        #:fiveam)
  (:import-from #:inga/analyzer
                #:find-ast
                #:analyzer-java
                #:ast-value
                #:start-analyzer
                #:stop-analyzer)
  (:import-from #:inga/ast-index
                #:ast-index-root-path
                #:ast-index-paths
                #:clean-indexes
                #:create-indexes
                #:get-ast)
  (:import-from #:inga/ast-index/disk
                #:ast-index-disk)
  (:import-from #:inga/contexts
                #:context-analyzers
                #:context-lc
                #:context-processes
                #:make-context)
  (:import-from #:inga/file
                #:convert-to-pos
                #:convert-to-top-offset
                #:get-file-type)
  (:import-from #:inga/language-client
                #:make-client
                #:start-client
                #:stop-client)
  (:export #:*index*
           #:jvm-ctx
           #:node-ctx
           #:find-ast-in-ctx
           #:create-range
           #:get-file-pos))
(in-package #:inga/test/helper)

(def-fixture jvm-ctx (root-path &key (index-type 'ast-index-disk) (include '("**")))
  (defparameter *root-path* root-path)
  (defparameter *index* (make-instance index-type :root-path root-path))
  (defparameter *ctx* (make-context
                        :kind :java
                        :project-path root-path
                        :include include
                        :ast-index *index*
                        :analyzers (list
                                     (start-analyzer :java include nil root-path *index*)
                                     (start-analyzer :kotlin include nil root-path *index*))
                        :processes (list
                                     (inga/plugin/spring/spring-property-loader:start root-path)
                                     (inga/plugin/jvm-dependency-loader:start root-path))))
  (unwind-protect
    (&body)
    (progn
      (loop for p in (context-processes *ctx*) do (uiop:close-streams p)) 
      (loop for a in (context-analyzers *ctx*) do (stop-analyzer a)))))

(def-fixture node-ctx (root-path &key (index-type 'ast-index-disk) (include '("**")))
  (defparameter *root-path* root-path)
  (defparameter *index* (make-instance index-type :root-path root-path))
  (defparameter *ctx* (make-context
                        :kind :typescript
                        :project-path root-path
                        :include include
                        :lc (make-client :typescript root-path)
                        :ast-index *index*
                        :analyzers (list
                                     (start-analyzer :typescript include nil root-path *index*))))
  (start-client (context-lc *ctx*))
  (unwind-protect
    (&body)
    (progn
      (stop-client (context-lc *ctx*)) 
      (loop for p in (context-processes *ctx*) do (uiop:close-streams p)) 
      (loop for a in (context-analyzers *ctx*) do (stop-analyzer a)))))

(defmacro find-ast-in-ctx (readable-pos &key (type nil))
  `(let* ((path (cdr (assoc :path ,readable-pos)))
          (pos (list (cons :path path)
                     (cons :top-offset
                           (convert-to-top-offset
                             (merge-pathnames path (ast-index-root-path *index*))
                             ,readable-pos))))
          (result (find-ast pos *index* :type ,type)))
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

(defun get-file-pos (result root-path)
  (let* ((entrypoint (cdr (assoc :entrypoint result)))
         (pos (if (eq (cdr (assoc :type entrypoint)) :rest-server)
                  (cdr (assoc :file-pos entrypoint))
                  entrypoint))
         (text-pos (convert-to-pos (merge-pathnames (cdr (assoc :path pos)) root-path)
                                   (cdr (assoc :top-offset pos)))))
    `((:type . ,(cdr (assoc :type result)))
      (:path . ,(cdr (assoc :path pos)))
      (:name . ,(cdr (assoc :name pos)))
      (:line . ,(cdr (assoc :line text-pos)))
      (:offset . ,(cdr (assoc :offset text-pos))))))

