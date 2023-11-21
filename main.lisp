(defpackage #:inga/main
  (:use #:cl
        #:inga/file
        #:inga/utils)
  (:import-from #:jsown)
  (:import-from #:alexandria)
  (:import-from #:inga/git
                #:get-diff)
  (:import-from #:inga/language-client
                #:make-client
                #:start-client
                #:stop-client
                #:references-client)
  (:import-from #:inga/ast-analyzer
                #:convert-to-pos
                #:convert-to-top-offset
                #:start-ast-analyzer
                #:stop-ast-analyzer
                #:find-definitions
                #:find-entrypoint
                #:find-references)
  (:import-from #:inga/ast-index
                #:ast-index-disk)
  (:import-from #:inga/plugin/jvm-dependency-loader)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path)
  (:import-from #:inga/plugin/spring-property-loader)
  (:import-from #:inga/utils
                #:split-trim-comma)
  (:import-from #:inga/errors
                #:inga-error)
  (:import-from #:inga/logger
                #:log-debug)
  (:export #:command))
(in-package #:inga/main)

(defparameter *include-typescript*
  '("*.(js|jsx)"
    "*.(ts|tsx)"))
(defparameter *include-java*
  '("*.java"
    "*.kt"))

(define-condition inga-error-option-not-found (inga-error) ())
(define-condition inga-error-context-not-found (inga-error) ())

(defun command (&rest argv)
  (handler-case
    (destructuring-bind (&key root-path include exclude github-token base-commit) (parse-argv argv)
      (let ((diffs (get-diff root-path base-commit)))
        (let ((ctx (start root-path
                          (filter-active-context (get-analysis-kinds diffs) (get-env-kinds))
                          :include include :exclude exclude)))
          (let ((results (analyze ctx diffs)))
            (ensure-directories-exist "reports/")
            (with-open-file (out "reports/report.json"
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (format out "~a" (to-json results root-path)))
            (format t "~%~a~%" (to-json results root-path)))
          (stop ctx))))
    (inga-error (e) (format t "~a~%" e))))

(defun parse-argv (argv)
  (loop with root-path = "."
        with include
        with exclude
        with github-token
        with base-commit
        for option = (pop argv)
        while option
        do (alexandria:switch (option :test #'equal)
             ("--root-path"
              (setf root-path (pop argv)))
             ("--front-path"
              (setf root-path (pop argv)))
             ("--back-path"
              (setf root-path (pop argv)))
             ("--include"
              (setf include (split-trim-comma (pop argv))))
             ("--exclude"
              (setf exclude (split-trim-comma (pop argv))))
             ("--github-token"
              (setf github-token (pop argv)))
             ("--base-commit"
              (setf base-commit (pop argv)))
             (t (error 'inga-error-option-not-found)))
        finally
          (return (append 
                    (list :root-path (truename (uiop:merge-pathnames* root-path)))
                    (list :include include)
                    (list :exclude exclude)
                    (when github-token
                      (list :github-token github-token))
                    (when base-commit
                      (list :base-commit base-commit))))))

(defun start (root-path context-kinds &key include exclude)
  (let* ((index (make-instance 'ast-index-disk :root-path root-path))
         (ctx (alexandria:switch ((when (> (length context-kinds) 0) (first context-kinds)))
               (:typescript
                 (make-context
                   :project-path root-path
                   :include (or include *include-typescript*)
                   :exclude exclude
                   :lc (make-client :typescript root-path)
                   :ast-index index
                   :ast-analyzers (list
                                    (start-ast-analyzer :typescript
                                                        (or include *include-typescript*)
                                                        exclude root-path index))))
               (:java
                 (make-context
                   :project-path root-path
                   :include (or include *include-java*)
                   :exclude exclude
                   :ast-index index
                   :ast-analyzers (list
                                    (start-ast-analyzer :java
                                                        (or include *include-java*)
                                                        exclude root-path index)
                                    (start-ast-analyzer :kotlin
                                                        (or include *include-java*)
                                                        exclude root-path index))
                   :processes (list
                                (inga/plugin/spring-property-loader:start root-path)
                                (inga/plugin/jvm-dependency-loader:start root-path))))
               (t (error 'inga-error-context-not-found)))))
    (start-client (context-lc ctx))
    ctx))

(defun stop (ctx)
  (stop-client (context-lc ctx)) 
  (loop for p in (context-processes ctx) do (uiop:close-streams p)) 
  (loop for a in (context-ast-analyzers ctx) do (stop-ast-analyzer a)))

(defun get-analysis-kinds (diffs)
  (remove nil
          (remove-duplicates
            (mapcar (lambda (diff)
                      (if (is-match (cdr (assoc :path diff)) *include-typescript*)
                          :typescript
                          (if (is-match (cdr (assoc :path diff)) *include-java*)
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
              (when (is-analysis-target (cdr (assoc :path range)) (context-include ctx) (context-exclude ctx))
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
                                                                    (cdr (assoc :origin pos))))
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
                                     (when (is-analysis-target (cdr (assoc :path ref))
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
                                                              (cdr (assoc :origin pos))))
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
                                                       (cdr (assoc :origin pos))))
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
                                                    (cdr (assoc :origin pos))))
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
  project-path
  include
  exclude
  lc
  ast-index
  ast-analyzers
  processes)

