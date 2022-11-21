(defpackage #:inga/main
  (:use #:cl
        #:inga/git
        #:inga/github
        #:inga/jsx
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
                #:make-parser
                #:start-parser
                #:stop-parser
                #:exec-parser
                #:find-affected-pos
                #:find-entrypoint)
  (:import-from #:inga/utils
                #:split-trim-comma)
  (:import-from #:inga/errors
                #:inga-error)
  (:import-from #:inga/logger
                #:log-debug)
  (:export #:command))
(in-package #:inga/main)

(defparameter *include-typescript*
  '("**/*.(js|jsx)"
    "**/*.(ts|tsx)"))

(defparameter *include-java*
  '("**/*.java"))

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
          (log-debug (format nil "pr: ~a~%" pr))
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

        (let ((ctx (start root-path
                          (filter-active-context (get-analysis-kinds diffs) (get-env-kinds))
                          exclude)))
          (let ((results (analyze ctx diffs)))
            (log-debug (format nil "results: ~a~%" results))
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
                   :lc (make-client :typescript root-path)
                   :parser (make-parser :typescript root-path)))
               (:java
                 (make-context
                   :project-path root-path
                   :include *include-java*
                   :exclude exclude
                   :lc (make-client :java root-path)
                   :parser (make-parser :java root-path)))
               (t (error 'inga-error-context-not-found)))))
    (start-client (context-lc ctx))
    (start-parser (context-parser ctx))
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
  (remove-duplicates
    (mapcan (lambda (range)
              (when (is-analysis-target (cdr (assoc :path range)) (context-include ctx) (context-exclude ctx))
                (analyze-by-range ctx range)))
            diffs)
    :test #'equal))

(defun analyze-by-range (ctx range)
  (let ((q (make-queue))
        (results '()))
    (enqueue q range)
    (loop
      (let ((range (dequeue q)))
        (if (null range) (return results))

        (let ((src-path (cdr (assoc :path range)))
              ast)
          (setf ast (exec-parser (context-parser ctx) src-path))
          (setf results
                (append results
                        (mapcan (lambda (pos)
                                  (find-entrypoints ctx pos q))
                                (get-affected-poss ctx ast src-path range)))))))))

(defun get-affected-poss (ctx ast src-path range)
  (remove nil
          (mapcar (lambda (line-no)
                    (let ((item-pos
                            (find-affected-pos (context-parser ctx)
                                               src-path ast line-no)))
                      (when item-pos
                        (log-debug (format nil "found-affected-pos: [src-path: ~a, line-no: ~a]~% -> ~a, " src-path line-no item-pos))
                        (when (assoc :origin range)
                          (setf item-pos (acons :origin (cdr (assoc :origin range)) item-pos)))
                        item-pos)))
                  (loop for line-no
                        from (cdr (assoc :start range))
                        to (cdr (assoc :end range))
                        collect line-no))))

(defun find-entrypoints (ctx pos q)
  (unless (assoc :origin pos)
    (push (cons :origin pos) pos))

  (let ((refs (references-client (context-lc ctx) pos))
        (results '()))
    (setf refs (remove nil (mapcar (lambda (ref)
                                     (when (is-analysis-target (cdr (assoc :path ref))
                                                               (context-include ctx)
                                                               (context-exclude ctx))
                                       ref))
                                   refs)))
    (log-debug (format nil "found-references: [pos: ~a]~% -> ~a, " pos refs))
    (if refs
        (loop for ref in refs
              do (progn 
                   (let ((entrypoint (find-entrypoint (context-parser ctx) ref)))
                     (if entrypoint
                         (setf results (append results
                                               (list
                                                 (list (cons :origin (cdr (assoc :origin pos)))
                                                       (cons :entorypoint entrypoint)))))
                         (enqueue q (list
                                      (cons :path (cdr (assoc :path ref)))
                                      (cons :origin (cdr (assoc :origin pos)))
                                      (cons :start (cdr (assoc :line ref)))
                                      (cons :end (cdr (assoc :line ref)))))))))
        (setf results
              (list
                (list
                  (cons :origin (cdr (assoc :origin pos)))
                  (cons :entorypoint
                            (list
                              (cons :path (enough-namestring (cdr (assoc :path pos))
                                                             (context-project-path ctx)))
                              (cons :name (cdr (assoc :name pos)))
                              (cons :line (cdr (assoc :line pos)))
                              (cons :offset (cdr (assoc :offset pos)))))))))
    results))

(defun inject-mark (project-path component-poss)
  (loop for pos in component-poss collect
        (let ((line-no 0))
              (with-open-file (instream (uiop:merge-pathnames* (cdr (assoc :path pos)) project-path))
                (with-open-file (outstream (uiop:merge-pathnames* (cdr (assoc :path pos)) project-path)
                                           :direction :output
                                           :if-exists :overwrite)
                  (loop for line = (read-line instream nil) while line collect
                        (progn
                          (setq line-no (+ line-no 1))
                          (if (= line-no (cdr (assoc :line pos)))
                              (format outstream "~a~%"
                                      (inject-mark-on-line line (cdr (assoc :offset pos)) 1))
                              (write-line line outstream)))))))))

(defstruct context
  project-path
  include
  exclude
  lc
  parser)

