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
                #:find-references
                #:create-indexes
                #:clean-indexes)
  (:import-from #:inga/cache
                #:make-cache
                #:size)
  (:import-from #:inga/plugin/jvm-dependency-loader)
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
(defparameter *cache-max-size* 100)
(defparameter *cache* (make-cache *cache-max-size*))

(defparameter *debug-parse* (inga/utils::make-measuring-time))
(defparameter *debug-find-definitions* (inga/utils::make-measuring-time))
(defparameter *debug-find-references* (inga/utils::make-measuring-time))

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
            (log-debug (format nil "cache size: ~a/~a" (size *cache*) *cache-max-size*))
            (log-debug (format nil "measuring time:~%  find-definitions: [times: ~a, avg-sec: ~f]~%  find-references: [times: ~a, avg-sec: ~f, cache-hit: ~a]"
                               (inga/utils::measuring-time-times *debug-find-definitions*)
                               (inga/utils::avg-sec *debug-find-definitions*)
                               (inga/utils::measuring-time-times *debug-find-references*)
                               (inga/utils::avg-sec *debug-find-references*)
                               (inga/utils::measuring-time-cache-hit *debug-find-references*)))
            (ensure-directories-exist "reports/")
            (with-open-file (out "reports/report.json"
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (format out "~a" (to-json results)))
            (format t "~%~a~%" (to-json results)))
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
  (setf inga/ast-analyzer/base::*cache* *cache*)
  (let ((ctx (alexandria:switch ((when (> (length context-kinds) 0) (first context-kinds)))
               (:typescript
                 (make-context
                   :project-path root-path
                   :include (or include *include-typescript*)
                   :exclude exclude
                   :lc (make-client :typescript root-path *cache*)
                   :ast-analyzers (list
                                    (start-ast-analyzer :typescript exclude root-path))))
               (:java
                 (make-context
                   :project-path root-path
                   :include (or include *include-java*)
                   :exclude exclude
                   :ast-analyzers (list
                                    (start-ast-analyzer :java exclude root-path)
                                    (start-ast-analyzer :kotlin exclude root-path))
                   :processes (list
                                (inga/plugin/spring-property-loader:start root-path)
                                (inga/plugin/jvm-dependency-loader:start root-path))))
               (t (error 'inga-error-context-not-found)))))
    (create-indexes root-path :include (context-include ctx) :exclude (context-exclude ctx))
    (start-client (context-lc ctx))
    ctx))

(defun stop (ctx)
  (loop for p in (context-processes ctx) do (uiop:close-streams p)) 
  (loop for a in (context-ast-analyzers ctx) do (stop-ast-analyzer a))
  (clean-indexes)
  (stop-client (context-lc ctx)))

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
                                      (context-project-path ctx)
                                      (cdr (assoc :path diff))
                                      (list
                                        (cons :line (cdr (assoc :start diff)))
                                        (cons :offset 0))))
                              diff)
                        (push (cons :end-offset
                                    (convert-to-top-offset
                                      (context-project-path ctx)
                                      (cdr (assoc :path diff))
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
                                        `(((:type . "server")
                                           (:origin .
                                            ,(convert-to-output-pos (context-project-path ctx)
                                                                    (cdr (assoc :origin pos))))
                                           (:entorypoint .
                                            ,(convert-to-output-pos (context-project-path ctx)
                                                                    pos)))))  
                                      (find-entrypoints ctx pos q)))
                            (inga/utils::measure
                              *debug-find-definitions*
                              (lambda () (find-definitions range)))))))))

(defun find-entrypoints (ctx pos q)
  (let ((refs
          (if (context-lc ctx)
              (inga/utils::measure
                *debug-find-references*
                (lambda () (references-client (context-lc ctx) pos))) 
              (inga/utils::measure
                *debug-find-references*
                (lambda () (find-references pos)))))
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
                                  `(((:type . "server")
                                     (:origin .
                                      ,(convert-to-output-pos (context-project-path ctx)
                                                              (cdr (assoc :origin pos))))
                                     (:entorypoint .
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
                                                   (:entorypoint .
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
              `(((:type . "server")
                 (:origin . ,(convert-to-output-pos (context-project-path ctx)
                                                    (cdr (assoc :origin pos))))
                 (:entorypoint . ,(convert-to-output-pos (context-project-path ctx) pos))))))
    results))

(defun convert-to-output-pos (root-path pos)
  (when (eq (cdr (assoc :type pos)) :rest-server)
    (setf pos (cdr (assoc :file-pos pos))))
  (let ((text-pos (convert-to-pos root-path (cdr (assoc :path pos)) (cdr (assoc :top-offset pos)))))
    (list
      (cons :path (enough-namestring (cdr (assoc :path pos)) root-path))
      (cons :name (cdr (assoc :name pos)))
      (cons :line (cdr (assoc :line text-pos)))
      (cons :offset (cdr (assoc :offset text-pos))))))

(defun to-json (results)
  (jsown:to-json
    (mapcan (lambda (r)
              `((:obj
                  ("type" . ,(cdr (assoc :type r)))
                  ("origin" . ,(cons :obj (key-downcase (cdr (assoc :origin r)))))
                  ("entorypoint" . ,(cons :obj (key-downcase (cdr (assoc :entorypoint r))))))))
            results)))

(defun key-downcase (obj)
  (mapcar (lambda (p) (cons (string-downcase (car p)) (cdr p))) obj))

(defstruct context
  project-path
  include
  exclude
  lc
  ast-analyzers
  processes)

