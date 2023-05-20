(defpackage #:inga/main
  (:use #:cl
        #:inga/git
        #:inga/github
        #:inga/file
        #:inga/utils)
  (:import-from #:jsown)
  (:import-from #:alexandria)
  (:import-from #:inga/language-client
                #:make-client
                #:start-client
                #:stop-client
                #:references-client)
  (:import-from #:inga/parser
                #:convert-to-pos
                #:convert-to-top-offset
                #:make-parser
                #:start-parser
                #:stop-parser
                #:parse
                #:exec-parser
                #:find-affected-poss
                #:find-entrypoint
                #:find-references)
  (:import-from #:inga/cache
                #:make-cache
                #:size)
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
(defparameter *debug-find-affected-poss* (inga/utils::make-measuring-time))
(defparameter *debug-find-refs* (inga/utils::make-measuring-time))

(define-condition inga-error-option-not-found (inga-error) ())

(define-condition inga-error-context-not-found (inga-error) ())

(defun command (&rest argv)
  (handler-case
    (destructuring-bind (&key root-path exclude github-token base-commit) (parse-argv argv)
      (let (diffs pr hostname)
        (when github-token
          (setf hostname (inga/git:get-hostname root-path))
          (inga/github:login hostname github-token)
          (setf pr (inga/github:get-pr root-path))
          (log-debug (format nil "pr: ~a" pr))
          (destructuring-bind (&key base-url owner-repo number merge-state-status base-ref-name head-sha) pr
            ;; https://docs.github.com/en/graphql/reference/enums#mergestatestatus
            (when (or
                    (string= merge-state-status "BEHIND")
                    (string= merge-state-status "DIRTY"))
              (log-debug (format nil "can't diff when merge state is not clean"))
              (return-from command))

            (unless base-commit
              (inga/git:track-branch base-ref-name root-path)
              (setf base-commit base-ref-name))))
        (setf diffs (get-diff root-path base-commit))

        (let ((ctx (time (start root-path
                                (filter-active-context (get-analysis-kinds diffs) (get-env-kinds))
                                exclude))))
          (let ((results (time (analyze ctx diffs))))
            (log-debug (format nil "cache size: ~a/~a" (size *cache*) *cache-max-size*))
            (log-debug (format nil "measuring time:~%  find-affected-poss: [times: ~a, avg-sec: ~f]~%  find-refs: [times: ~a, avg-sec: ~f, cache-hit: ~a]"
                               (inga/utils::measuring-time-times *debug-find-affected-poss*)
                               (inga/utils::avg-sec *debug-find-affected-poss*)
                               (inga/utils::measuring-time-times *debug-find-refs*)
                               (inga/utils::avg-sec *debug-find-refs*)
                               (inga/utils::measuring-time-cache-hit *debug-find-refs*)))
            (ensure-directories-exist "reports/")
            (with-open-file (out "reports/report.json"
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (format out "~a" (to-json results)))
            (format t "~%~a~%" (to-json results))
            (when (and pr results)
              (inga/github:send-pr-comment hostname pr results root-path)))
          (stop ctx))))
    (inga-error (e) (format t "~a~%" e))))

(defun parse-argv (argv)
  (loop with root-path = "."
        with exclude = '()
        with github-token = nil
        with base-commit = nil
        for option = (pop argv)
        while option
        do (alexandria:switch (option :test #'equal)
             ("--root-path"
              (setf root-path (pop argv)))
             ("--front-path"
              (setf root-path (pop argv)))
             ("--back-path"
              (setf root-path (pop argv)))
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
                    (list :exclude exclude)
                    (when github-token
                      (list :github-token github-token))
                    (when base-commit
                      (list :base-commit base-commit))))))

(defun start (root-path context-kinds &optional (exclude '()))
  (let ((ctx (alexandria:switch ((when (> (length context-kinds) 0) (first context-kinds)))
               (:typescript
                 (make-context
                   :project-path root-path
                   :include *include-typescript*
                   :exclude exclude
                   :lc (make-client :typescript root-path *cache*)
                   :parser (make-parser :typescript root-path *cache*)))
               (:java
                 (make-context
                   :project-path root-path
                   :include *include-java*
                   :exclude exclude
                   :parser (make-parser :java root-path *cache*)))
               (t (error 'inga-error-context-not-found)))))
    (start-client (context-lc ctx))
    (start-parser (context-parser ctx) (context-include ctx) (context-exclude ctx))
    ctx))

(defun stop (ctx)
  (stop-parser (context-parser ctx)) 
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
                                        (cons :offset 0)))) diff)
                        (push (cons :end-offset
                                    (convert-to-top-offset
                                      (context-project-path ctx)
                                      (cdr (assoc :path diff))
                                      (list
                                        (cons :line (cdr (assoc :end diff)))
                                        (cons :offset -1)))) diff))
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
                              (find-entrypoints ctx pos q))
                            (inga/utils::measure
                              *debug-find-affected-poss*
                              (lambda () (find-affected-poss (context-parser ctx) range)))))))))

(defun find-entrypoints (ctx pos q)
  (let ((refs (inga/utils::measure
                *debug-find-refs*
                (lambda () (find-references (context-parser ctx) pos))))
        results)
    (unless refs
      (setf refs (inga/utils::measure
                   *debug-find-refs*
                   (lambda () (references-client (context-lc ctx) pos)))))

    (setf refs (remove nil (mapcar (lambda (ref)
                                     (when (is-analysis-target (cdr (assoc :path ref))
                                                               (context-include ctx)
                                                               (context-exclude ctx))
                                       ref))
                                   refs)))
    (if refs
        (loop for ref in refs
              do
              (let ((entrypoint (find-entrypoint (context-parser ctx) ref)))
                (if entrypoint
                    (setf results
                          (append results
                                  (list
                                    (list (cons :origin
                                                (convert-to-output-pos (context-project-path ctx)
                                                                       (cdr (assoc :origin pos))))
                                          (cons :entorypoint
                                                (convert-to-output-pos (context-project-path ctx) entrypoint))))))
                    (enqueue q (list
                                 (cons :path (cdr (assoc :path ref)))
                                 (cons :origin (cdr (assoc :origin pos)))
                                 (cons :start-offset (cdr (assoc :top-offset ref)))
                                 (cons :end-offset (cdr (assoc :top-offset ref))))))))
        (setf results
              (list
                (list
                  (cons :origin
                        (convert-to-output-pos (context-project-path ctx)
                                               (cdr (assoc :origin pos))))
                  (cons :entorypoint
                        (convert-to-output-pos (context-project-path ctx)
                                               pos))))))
    results))

(defun convert-to-output-pos (root-path pos)
  (let ((text-pos (convert-to-pos root-path (cdr (assoc :path pos)) (cdr (assoc :top-offset pos)))))
    (list
      (cons :path (enough-namestring (cdr (assoc :path pos)) root-path))
      (cons :name (cdr (assoc :name pos)))
      (cons :line (cdr (assoc :line text-pos)))
      (cons :offset (cdr (assoc :offset text-pos))))))

(defun to-json (results)
  (jsown:to-json
    (mapcan (lambda (r)
              (list (cons :obj (list
                                 (cons "origin"
                                       (cons :obj (key-downcase (cdr (assoc :origin r)))))
                                 (cons "entorypoint"
                                       (cons :obj (key-downcase (cdr (assoc :entorypoint r)))))))))
            results)))

(defun key-downcase (obj)
  (mapcar (lambda (p) (cons (string-downcase (car p)) (cdr p))) obj))

(defstruct context
  project-path
  include
  exclude
  lc
  parser)

