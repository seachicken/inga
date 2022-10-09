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
                #:initialize-client
                #:references-client)
  (:import-from #:inga/parser
                #:make-parser
                #:start-parser
                #:stop-parser
                #:exec-parser
                #:find-affected-pos
                #:find-entrypoint)
  (:export #:command))
(in-package #:inga/main)

(defparameter *include-typescript*
  '("*.js" "*.jsx"
    "*.ts" "*.tsx"))

(defparameter *include-java*
  '("*.java"))

(define-condition inga-error (error) ())

(define-condition inga-error-context-not-found (inga-error) ())

(defun command (&rest argv)
  (destructuring-bind (&key root-path exclude github-token base-sha) (parse-argv argv)
    (let (diffs pr hostname)
      (when github-token
        (setf hostname (inga/git:get-hostname root-path))
        (inga/github:login hostname github-token)
        (setf pr (inga/github:get-pr root-path))
        (destructuring-bind (&key base-url owner-repo number base-ref-name head-sha) pr
          (unless base-sha
            (inga/git:track-branch base-ref-name root-path)
            (setf base-sha base-ref-name))))
      (setf diffs (get-diff root-path base-sha))

      (handler-case
        (let ((ctx (start root-path (get-analysis-kinds diffs) exclude)))
          (let ((results (analyze ctx diffs)))
            (format t "~a~%" results)
            (when (and pr results)
              (destructuring-bind (&key base-url owner-repo number base-ref-name head-sha) pr
                (inga/github:send-pr-comment hostname base-url owner-repo number results root-path head-sha))))
          (stop ctx))
        (error (e) (format t "~a~%" e))))))

(defun parse-argv (argv)
  (loop with root-path = "."
        with exclude = '()
        with github-token = nil
        with base-sha = nil
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
              (setf exclude
                    (append exclude
                            (mapcar (lambda (exc)
                                      (string-trim '(#\Space) exc))
                                    (split #\,
                                           (pop argv))))))
             ("--github-token"
              (setf github-token (pop argv)))
             ("--base-sha"
              (setf base-sha (pop argv))))
        finally
          (return (append 
                    (list :root-path (truename (uiop:merge-pathnames* root-path)))
                    (list :exclude exclude)
                    (when github-token
                      (list :github-token github-token))
                    (when base-sha
                      (list :base-sha base-sha))))))

(defun split (div sequence)
  (let ((pos (position div sequence)))
    (if pos
        (list* (subseq sequence 0 pos)
               (split div (subseq sequence (1+ pos))))
        (list sequence))))

(defun start (root-path kinds &optional (exclude '()))
  (alexandria:switch ((when (> (length kinds) 0) (first kinds)))
    (:typescript
      (let ((ctx (make-context
                   :project-path root-path
                   :include *include-typescript*
                   :exclude exclude
                   :lc (make-client :typescript root-path)
                   :parser (make-parser :typescript root-path))))
        (start-client (context-lc ctx))
        (start-parser (context-parser ctx)) 
        ctx))
    (:java
      (let ((ctx (make-context
                   :project-path root-path
                   :include *include-java*
                   :exclude exclude
                   :lc (make-client :java root-path)
                   :parser (make-parser :java root-path))))
        (start-client (context-lc ctx))
        (initialize-client (context-lc ctx))
        (start-parser (context-parser ctx)) 
        ctx))
    (t (error 'inga-error-context-not-found))))

(defun stop (ctx)
  (stop-parser (context-parser ctx)) 
  (stop-client (context-lc ctx)))

(defun get-analysis-kinds (diffs)
  (remove nil
          (mapcar (lambda (diff)
                    (if (is-match (cdr (assoc :path diff)) *include-typescript*)
                        :typescript
                        (if (is-match (cdr (assoc :path diff)) *include-java*)
                            :java
                            nil)))
                  diffs)))

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
  (remove-duplicates
    (remove nil
            (mapcar (lambda (line-no)
                      (let ((item-pos
                              (find-affected-pos (context-parser ctx)
                                                 src-path ast line-no)))
                        (when item-pos
                          (acons :path src-path item-pos))))
                    (loop for line-no
                          from (cdr (assoc :start range))
                          to (cdr (assoc :end range))
                          collect line-no)))
    :test #'equal))

(defun find-entrypoints (ctx pos q)
  (let ((refs (references-client (context-lc ctx) pos))
        (results '()))
    (setf refs (remove nil (mapcar (lambda (ref)
                                     (when (is-analysis-target (cdr (assoc :path ref))
                                                               (context-include ctx)
                                                               (context-exclude ctx))
                                       ref))
                                   refs)))
    (if refs
        (loop for ref in refs
              do (let ((entrypoint (find-entrypoint (context-parser ctx)
                                                    ref)))
                   (if entrypoint
                       (setf results (append results (list entrypoint)))
                       (enqueue q (list
                                    (cons :path (cdr (assoc :path ref)))
                                    (cons :start (cdr (assoc :line ref)))
                                    (cons :end (cdr (assoc :line ref))))))))
        (setf results
              (list
                (list (cons :path (enough-namestring (cdr (assoc :path pos))
                                                     (context-project-path ctx)))
                      (cons :name (cdr (assoc :name pos)))
                      (cons :line (cdr (assoc :line pos)))
                      (cons :offset (cdr (assoc :offset pos)))))))
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

