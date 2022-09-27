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

(defun command (&rest argv)
  (destructuring-bind (&key front-path back-path exclude github-token base-sha inject-mark) (parse-argv argv)
    (multiple-value-bind (front back)
      (start :front-path front-path :back-path back-path :exclude exclude)

      (let (project-path pr hostname)
        (if back-path
          (setf project-path back-path)
          (setf project-path front-path))

        (when github-token
          (setf hostname (inga/git:get-hostname project-path))
          (inga/github:login hostname github-token)
          (setf pr (inga/github:get-pr project-path))
          (destructuring-bind (&key base-url owner-repo number base-ref-name head-sha last-report) pr
            (unless base-sha
              (inga/git:track-branch base-ref-name project-path)
              (setf base-sha base-ref-name))))

        (let ((results (if back-path
                           (analyze back base-sha)
                           (analyze front base-sha))))
          (format t "~a~%" results)
          (when pr
            (destructuring-bind (&key base-url owner-repo number base-ref-name head-sha last-report) pr
              (inga/github:send-pr-comment hostname base-url owner-repo number results project-path
                                           (if back-path t nil)
                                           head-sha last-report)))
          (when inject-mark
            (inject-mark front-path results))))
      (stop front back))))

(defun parse-argv (argv)
  (loop with front-path = nil
        with back-path = nil
        with exclude = '()
        with github-token = nil
        with base-sha = nil
        with inject-mark = nil
        for option = (pop argv)
        while option
        do (alexandria:switch (option :test #'equal)
             ("--front-path"
              (setf front-path (pop argv)))
             ("--back-path"
              (setf back-path (pop argv)))
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
              (setf base-sha (pop argv)))
             ("--inject-mark"
              (setf inject-mark t)))
        finally
          (return (append 
                    (when front-path
                      (list :front-path (truename (uiop:merge-pathnames* front-path))))
                    (when back-path
                      (list :back-path (truename (uiop:merge-pathnames* back-path))))
                    (list :exclude exclude)
                    (when github-token
                      (list :github-token github-token))
                    (when base-sha
                      (list :base-sha base-sha))
                    (list :inject-mark inject-mark)))))

(defun split (div sequence)
  (let ((pos (position div sequence)))
    (if pos
        (list* (subseq sequence 0 pos)
               (split div (subseq sequence (1+ pos))))
        (list sequence))))

(defun start (&key front-path back-path (exclude '()))
  (let (front back)
    (when front-path
      (setf front (make-context
                    :kind :front
                    :project-path front-path
                    :include '("*.js" "*.jsx"
                               "*.ts" "*.tsx")
                    :exclude exclude
                    :lc (make-client :typescript front-path)
                    :parser (make-parser :typescript front-path)))
      (start-client (context-lc front))
      (start-parser (context-parser front)))
    (when back-path
      (setf back (make-context
                   :kind :back
                   :project-path back-path
                   :include '("*.java")
                   :exclude exclude
                   :lc (make-client :java back-path)
                   :parser (make-parser :java back-path)))
      (start-client (context-lc back))
      (initialize-client (context-lc back))
      (start-parser (context-parser back)))
    (values front back)))

(defun stop (front-ctx back-ctx)
  (when front-ctx
    (stop-parser (context-parser front-ctx)) 
    (stop-client (context-lc front-ctx)))
  (when back-ctx
    (stop-parser (context-parser back-ctx))
    (stop-client (context-lc back-ctx))))

(defun get-val (list key)
  (loop for item in list do
        (when (equal (car item) key)
          (return-from get-val (cdr item)))))

(defun analyze (ctx base-sha)
  (remove-duplicates
    (mapcan (lambda (range)
              (analyze-by-range ctx range))
            (get-diff (context-project-path ctx) base-sha (context-include ctx)))
    :test #'equal))

(defun analyze-by-range (ctx range)
  (let ((q (make-queue))
        (results '()))
    (enqueue q range)
    (loop
      (let ((range (dequeue q)))
        (if (null range) (return results))

        (let ((src-path (get-val range "path"))
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
                          from (get-val range "start")
                          to (get-val range "end")
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
                                    (cons "path" (cdr (assoc :path ref)))
                                    (cons "start" (cdr (assoc :line ref)))
                                    (cons "end" (cdr (assoc :line ref))))))))
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
  kind
  project-path
  include
  exclude
  lc
  parser)

