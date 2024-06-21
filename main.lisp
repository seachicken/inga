(defpackage #:inga/main
  (:use #:cl
        #:inga/file
        #:inga/utils)
  (:import-from #:jsown)
  (:import-from #:alexandria)
  (:import-from #:inga/context
                #:*default-mode*)
  (:import-from #:inga/git
                #:get-diff)
  (:import-from #:inga/language-client
                #:make-client
                #:start-client
                #:stop-client
                #:references-client)
  (:import-from #:inga/traversal
                #:convert-to-pos
                #:convert-to-top-offset
                #:start-traversal
                #:stop-traversal
                #:find-definitions
                #:find-entrypoint
                #:find-references)
  (:import-from #:inga/ast-index
                #:ast-index-disk)
  (:import-from #:inga/plugin/jvm-dependency-loader)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path)
  (:import-from #:inga/plugin/spring/spring-property-loader)
  (:import-from #:inga/utils
                #:split-trim-comma)
  (:import-from #:inga/errors
                #:inga-error)
  (:import-from #:inga/logger
                #:log-debug)
  (:export #:*mode*
           #:parse-argv
           #:command
           #:analyze
           #:convert-to-output-pos  
           #:to-json
           #:key-downcase
           #:context
           #:context-ast-index))
(in-package #:inga/main)

(define-condition inga-error-option-not-found (inga-error) ())
(define-condition inga-error-context-not-found (inga-error) ())

(defun parse-argv (argv)
  (loop with root-path = "."
        with temp-path
        with include
        with exclude
        with base-commit
        with mode = *default-mode*
        for option = (pop argv)
        while option
        do (alexandria:switch (option :test #'equal)
             ("--root-path"
              (setf root-path (pop argv)))
             ("--front-path"
              (setf root-path (pop argv)))
             ("--back-path"
              (setf root-path (pop argv)))
             ("--temp-path"
              (setf temp-path (pop argv)))
             ("--include"
              (setf include (split-trim-comma (pop argv))))
             ("--exclude"
              (setf exclude (split-trim-comma (pop argv))))
             ("--base-commit"
              (setf base-commit (pop argv)))
             ("--mode"
              (setf mode (intern (string-upcase (pop argv)) :keyword)))
             (t (error 'inga-error-option-not-found)))
        finally
          (return (append 
                    (list :root-path (truename (uiop:merge-pathnames* root-path)))
                    (list :temp-path
                          (if temp-path
                              (pathname (concatenate 'string temp-path "/"))
                              (merge-pathnames ".inga/")))
                    (list :include include)
                    (list :exclude exclude)
                    (when base-commit
                      (list :base-commit base-commit))
                    (list :mode mode)))))

(defun command (params)
  (handler-case
    (destructuring-bind (&key root-path temp-path include exclude base-commit mode) params
      (let* ((diffs (get-diff root-path base-commit))
             (ctx (start root-path
                         (filter-active-context (get-analysis-kinds diffs) (get-env-kinds))
                         :include include :exclude exclude :temp-path temp-path))
             (results (analyze ctx diffs)))
        (ensure-directories-exist (merge-pathnames "report/" temp-path))
        (with-open-file (out (merge-pathnames "report/report.json" temp-path)
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (format out "~a" (to-json results root-path)))
        (format t "~%~a~%" (to-json results root-path))
        (stop ctx)))
    (inga-error (e) (format t "~a~%" e))))

(defun start (root-path context-kinds &key include exclude (temp-path (merge-pathnames ".inga/")))
  (let* ((index (make-instance 'ast-index-disk :root-path root-path :temp-path temp-path))
         (ctx (alexandria:switch ((when (> (length context-kinds) 0) (first context-kinds)))
               (:typescript
                 (make-context
                   :kind :typescript
                   :project-path root-path
                   :include include
                   :exclude exclude
                   :lc (make-client :typescript root-path)
                   :ast-index index
                   :traversals (list
                                 (start-traversal :typescript
                                                  include
                                                  exclude root-path index))))
               (:java
                 (make-context
                   :kind :java
                   :project-path root-path
                   :include include
                   :exclude exclude
                   :ast-index index
                   :traversals (list
                                 (start-traversal :java
                                                  include
                                                  exclude root-path index)
                                 (start-traversal :kotlin
                                                  include
                                                  exclude root-path index))
                   :processes (list
                                (inga/plugin/spring/spring-property-loader:start root-path)
                                (inga/plugin/jvm-dependency-loader:start root-path))))
               (t (error 'inga-error-context-not-found)))))
    (start-client (context-lc ctx))
    ctx))

(defun stop (ctx)
  (stop-client (context-lc ctx)) 
  (loop for p in (context-processes ctx) do (uiop:close-streams p)) 
  (loop for a in (context-traversals ctx) do (stop-traversal a)))

(defun get-analysis-kinds (diffs)
  (remove nil
          (remove-duplicates
            (mapcar (lambda (diff)
                      (if (is-match (cdr (assoc :path diff)) :typescript)
                          :typescript
                          (if (is-match (cdr (assoc :path diff)) :java)
                              :java
                              nil)))
                    diffs))))

(defun get-env-kinds ()
  (let ((kinds (split-trim-comma (uiop:getenv "INGA_CONTEXT"))))
    (remove nil
            (remove-duplicates
              (mapcar (lambda (kind)
                        (alexandria:switch (kind :test #'equal)
                          ("typescript" :typescript)
                          ("java" :java)))
                      kinds)))))

(defun filter-active-context (found-context env-context)
  (log-debug (format nil "found context: ~a, env context: ~a" found-context env-context))
  (if env-context
      (mapcar (lambda (kind)
                (when (find kind found-context) kind))
              env-context)
      found-context))

(defun analyze (ctx diffs)
  (setf diffs (mapcar (lambda (diff)
                        (push (cons :start-offset
                                    (convert-to-top-offset
                                      (merge-pathnames
                                        (cdr (assoc :path diff)) 
                                        (context-project-path ctx))
                                      (list
                                        (cons :line (cdr (assoc :start diff)))
                                        (cons :offset 0))))
                              diff)
                        (push (cons :end-offset
                                    (convert-to-top-offset
                                      (merge-pathnames
                                        (cdr (assoc :path diff)) 
                                        (context-project-path ctx))
                                      (list
                                        (cons :line (cdr (assoc :end diff)))
                                        (cons :offset -1))))
                              diff))
                      diffs))
  (remove-duplicates
    (mapcan (lambda (range)
              (when (is-analysis-target (context-kind ctx)
                                        (cdr (assoc :path range))
                                        (context-include ctx) (context-exclude ctx))
                (analyze-by-range ctx range)))
            diffs)
    :test #'equal))

(defun analyze-by-range (ctx range)
  (let ((q (make-queue))
        results)
    (enqueue q range)
    (loop
      (setf range (dequeue q))
      (if (null range) (return results))

      (setf results
            (append results
                    (mapcan (lambda (pos)
                              (unless (assoc :origin pos)
                                (push (cons :origin pos) pos))
                              (append (when (eq (cdr (assoc :type pos)) :rest-server)
                                        `(((:type . "entrypoint")
                                           (:origin .
                                            ,(convert-to-output-pos (context-project-path ctx)
                                                                    (if (cdr (assoc :origin pos))
                                                                        (cdr (assoc :origin pos))
                                                                        pos)))
                                           (:entrypoint .
                                            ,(convert-to-output-pos (context-project-path ctx)
                                                                    pos)))))  
                                      (find-entrypoints ctx pos q)))
                            (find-definitions range)))))))

(defun find-entrypoints (ctx pos q)
  (let ((refs
          (if (context-lc ctx)
              (references-client (context-lc ctx) pos) 
              (find-references pos (context-ast-index ctx))))
        results)
    (setf refs (remove nil (mapcar (lambda (ref)
                                     (when (is-analysis-target (context-kind ctx)
                                                               (cdr (assoc :path ref))
                                                               (context-include ctx)
                                                               (context-exclude ctx))
                                       ref))
                                   refs)))
    (if refs
        (loop for ref in refs
              do
              (let ((entrypoint (find-entrypoint ref)))
                (if entrypoint
                    (setf results
                          (append results
                                  `(((:type . "entrypoint")
                                     (:origin .
                                      ,(convert-to-output-pos (context-project-path ctx)
                                                              (if (cdr (assoc :origin pos))
                                                                  (cdr (assoc :origin pos))
                                                                  entrypoint)))
                                     (:entrypoint .
                                      ,(convert-to-output-pos (context-project-path ctx) entrypoint))))))
                    (let ((origin
                            (if (eq (cdr (assoc :type pos)) :rest-server)
                                (let ((definition
                                        (first
                                          (find-definitions
                                            `((:path . ,(cdr (assoc :path ref)))
                                              (:start-offset . ,(cdr (assoc :top-offset ref)))
                                              (:end-offset . ,(cdr (assoc :top-offset ref))))))))
                                  (setf results
                                        (append results
                                                `(((:type . "connection")
                                                   (:origin .
                                                    ,(convert-to-output-pos
                                                       (context-project-path ctx)
                                                       (cdr (assoc :file-pos pos))))
                                                   (:entrypoint .
                                                    ,(convert-to-output-pos
                                                       (context-project-path ctx)
                                                       definition))))))
                                  definition)
                                (cdr (assoc :origin pos)))))
                      (enqueue q (list
                                   (cons :path (cdr (assoc :path ref)))
                                   (cons :origin origin)
                                   (cons :start-offset (cdr (assoc :top-offset ref)))
                                   (cons :end-offset (cdr (assoc :top-offset ref)))))))))
        (setf results
              `(((:type . "entrypoint")
                 (:origin . ,(convert-to-output-pos (context-project-path ctx)
                                                    (if (cdr (assoc :origin pos))
                                                        (cdr (assoc :origin pos))
                                                        pos)))
                 (:entrypoint . ,(convert-to-output-pos (context-project-path ctx) pos))))))
    results))

(defun convert-to-output-pos (root-path pos)
  (when (eq (cdr (assoc :type pos)) :rest-server)
    (setf pos (cdr (assoc :file-pos pos))))
  (let ((text-pos (convert-to-pos (merge-pathnames (cdr (assoc :path pos)) root-path)
                                  (cdr (assoc :top-offset pos)))))
    (list
      (cons :path (enough-namestring (cdr (assoc :path pos)) root-path))
      (cons :name (cdr (assoc :name pos)))
      (cons :line (cdr (assoc :line text-pos)))
      (cons :offset (cdr (assoc :offset text-pos))))))

(defun to-json (results root-path)
  (jsown:to-json
    (mapcan (lambda (r)
              (let ((obj
                      `((:obj
                          ("type" . ,(cdr (assoc :type r)))
                          ("origin" . ,(cons :obj (key-downcase (cdr (assoc :origin r)))))
                          ("entrypoint" . ,(cons :obj (key-downcase (cdr (assoc :entrypoint r)))))))))
                (when (equal (cdr (assoc :type r)) "entrypoint")
                  (push (cons "service" (first (last (pathname-directory
                                                       (find-base-path
                                                         (merge-pathnames
                                                           (cdr (assoc :path
                                                                       (cdr (assoc :entrypoint r))))
                                                           root-path))))))
                        (cdr (assoc :obj obj))))
                obj))
            results)))

(defun key-downcase (obj)
  (mapcar (lambda (p) (cons (string-downcase (car p)) (cdr p))) obj))

(defstruct context
  kind
  project-path
  include
  exclude
  lc
  ast-index
  traversals
  processes)

