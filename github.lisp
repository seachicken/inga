(defpackage #:inga/github
  (:use #:cl)
  (:import-from #:jsown)
  (:export #:login
           #:get-pr
           #:send-pr-comment))
(in-package #:inga/github)

(defun login (hostname token)
  (uiop:run-program (format nil "gh auth login --hostname ~a --with-token" hostname)
                    :input
                    (uiop:process-info-output
                      (uiop:launch-program (list "echo" token)
                                           :output :stream))
                    :output :string))

(defun get-pr (project-path)
  (let ((json (jsown:parse
                (uiop:run-program (format nil
                                          "(cd ~a && gh pr view --json url --json number --json baseRefName --json commits --json comments)"
                                          project-path)
                                  :output :string
                                  :ignore-error-status t))))
    (if (jsown:keyp json "url")
      (append
        (list :base-url
              (get-base-url (jsown:val json "url")))
        (list :owner-repo
              (get-owner-repo (jsown:val json "url")))
        (list :number
              (jsown:val json "number"))
        (list :base-ref-name
              (jsown:val json "baseRefName"))
        (list :head-sha
              (jsown:val (car (last (jsown:val json "commits"))) "oid"))
        (list :last-report
              (get-last-report (jsown:val json "comments"))))
      '())))

;; url format: https://HOST/OWNER/REPO/pull/NUMBER
(defun get-base-url (url)
  (let ((paths (split #\/ url)))
    (format nil "~{~a/~}" (subseq paths 0 (- (length paths) 2)))))

(defun get-owner-repo (url)
  (let ((paths (split #\/ url)) result)
    (setf result (format nil "~{~a/~}" (subseq paths 3 5)))
    (subseq result 0 (- (length result) 1))))

(defun get-last-report (comments)
  (loop
    with last-report
    for comment in comments
    do (let ((body (jsown:val comment "body")))
         (when (and (>= (length body) 13) (string= (subseq body 0 13) "# Inga Report"))
             (setf last-report body)))
    finally (return last-report)))

(defun send-pr-comment (hostname base-url owner-repo number affected-poss project-path is-back sha last-report)
  (let ((comment (format nil
                         "~a~%**~a affected by the change**~a~%~%<details><summary>Affected files</summary>~%~%~a~%</details>"
                         "# Inga Report"
                         (get-affected-display-name affected-poss is-back)
                         " (powered by [Inga](https://github.com/seachicken/inga))"
                         (get-code-hierarchy base-url sha affected-poss))))
    (when (not (string= comment last-report))
      (uiop:run-program (format nil
                                "(cd ~a && gh pr comment ~a -R ~a/~a --body '~a')"
                                project-path
                                number
                                hostname owner-repo
                                comment)
                        :output :string))))

(defun get-affected-display-name (affected-poss is-back)
  (if (equal (length affected-poss) 1)
      (if is-back "A entrypoint" "A component")
      (format nil
              (if is-back "~a entrypoints" "~a components")
              (length affected-poss))))

(defun get-code-hierarchy (base-url sha poss)
  (setf poss (mapcar (lambda (p)
                       (acons :paths (split #\/ (cdr (assoc :path p))) p))
                     poss))
  (let ((sorted-poss (group-by-dir (sort-flat base-url sha poss '() 0))) (result ""))
    (loop
      with i = 0
      with prev-dirs
      for poss in sorted-poss
      do (let ((dirs (get-dirs (cdr (assoc :paths (first poss)))))
               (prev-dirs (if (> i 0 )
                              (get-dirs (cdr (assoc :paths (car (nth (- i 1) sorted-poss)))))   
                              nil))
               (next-dirs (if (= (+ i 1) (length sorted-poss))
                              nil
                              (get-dirs (cdr (assoc :paths (car (nth (+ i 1) sorted-poss))))))))
           (multiple-value-bind (dirs-result num-of-nested) (output-dirs prev-dirs dirs next-dirs)
             (setf result
                   (format nil "~a~a" result dirs-result))

             (loop
               for pos in poss
               do (progn
                    (let ((file (get-file (cdr (assoc :paths pos))))
                          (name (cdr (assoc :name pos)))
                          (path (cdr (assoc :path pos)))
                          (line (cdr (assoc :line pos))))
                      (setf result
                            (format nil "~a~a~%"
                                    result
                                    (output-file file name
                                                 num-of-nested
                                                 base-url sha path line)))))))
           (setf i (+ i 1))))
    result))

(defun output-dirs (prev current next)
  (when (= (length current) 0)
    (return-from output-dirs (values "" 0)))

  (let ((prev-diff-i (get-diff-index current prev))
        (next-diff-i (get-diff-index current next))
        (result "")
        (nested-i 0))
    (loop
      with i = 0
      for path in current
      do (progn
           (if (< i prev-diff-i)
               (when (= (+ i 1) prev-diff-i)
                 (setf nested-i (+ nested-i 1)))
               (if (or (= next-diff-i 0) (< i next-diff-i))
                   (if (= (length result) 0)
                       (progn
                         (if (= nested-i 0)
                             (setf result (format nil "- 📂 ~a" path))
                             (setf result (format nil "~vt- 📂 ~a" (* nested-i 2) path)))
                         (setf nested-i (+ nested-i 1)))
                       (setf result (format nil "~a/~a" result path)))
                   (progn
                     (if (= (length result) 0)
                         (setf result (format nil "~vt- 📂 ~a" (* nested-i 2) path))
                         (setf result (format nil "~a~%~vt- 📂 ~a" result (* nested-i 2) path)))
                     (setf nested-i (+ nested-i 1)))))
           (setf i (+ i 1))))
    (values (if (= (length result) 0)
                result
                (format nil "~a~%" result))
            nested-i)))

(defun output-file (file name i base-url sha path line)
  (if (= i 0)
      (format nil "- 📄 [~a - ~a](~ablob/~a/~a#L~a)"
              file name
              base-url sha path line)
      (format nil "~vt- 📄 [~a - ~a](~ablob/~a/~a#L~a)"
              (* i 2)
              file name
              base-url sha path line)))

(defun group-by-dir (sorted-poss)
  (let ((results '()))
    (loop
      with same-dirs = '()
      for pos in sorted-poss
      do (let ((dirs (get-dirs (cdr (assoc :paths pos))))
               (file (get-file (cdr (assoc :paths pos))))
               (path (cdr (assoc :path pos)))
               (line (cdr (assoc :line pos))))
           (when (and
                   (> (length same-dirs) 0)
                   (not (equal dirs (get-dirs (cdr (assoc :paths (car (last same-dirs))))))))
             (setf results (append results (list same-dirs)))
             (setf same-dirs '()))
           (setf same-dirs (append same-dirs (list pos))))
      finally (setf results (append results (list same-dirs))))
    results))

(defun sort-flat (base-url sha poss results ri)
  (let ((files '()) (remaining-poss '()))
    (loop
      for pos in poss
      do (let ((paths (cdr (assoc :paths pos))))
           (if (= (length (subseq paths ri (length paths))) 1)
               (push pos files)
               (push pos remaining-poss))))

    (setf files (sort files #'string< :key
                      #'(lambda (f) (format nil "~a~a"
                                            (nth ri (cdr (assoc :paths f)))
                                            (cdr (assoc :line f))))))
    (setf results (append files results))

    (when (= (length remaining-poss) 0)
      (return-from sort-flat results))

    ;; sort by same directory
    (loop
      with prev-dir
      with group = '()
      for pos in remaining-poss
      do (let ((dir (nth ri (cdr (assoc :paths pos)))))
           (unless prev-dir
             (setf prev-dir dir))

           (when (not (string= dir prev-dir))
             (setf group (sort group #'string< :key
                               #'(lambda (p) (nth (+ ri 1) (cdr (assoc :paths p))))))
             (setf results (sort-flat base-url sha group results (+ ri 1)))
             (setf group '()))
           (push pos group)
           (setf prev-dir dir))
      finally (progn
                 (setf group (sort group #'string< :key
                                   #'(lambda (p) (nth (+ ri 1) (cdr (assoc :paths p))))))
                 (setf results (sort-flat base-url sha group results (+ ri 1))))))
     results)

(defun get-diff-index (a b)
  (when (or (= (length a) 0) (= (length b) 0))
            (return-from get-diff-index 0))
  (loop
    for i from 0 below (max (length a) (length b))
    collect (if (and (< i (length a)) (< i (length b)))
                (when (not (equal (subseq a 0 (+ i 1)) (subseq b 0 (+ i 1))))
                  (return-from get-diff-index i))
                (return-from get-diff-index i)))
  0)

(defun get-dirs (paths)
  (when (> (length paths) 0))
    (subseq paths 0 (- (length paths) 1)))

(defun get-file (paths)
  (first (subseq paths (- (length paths) 1) (length paths))))

(defun split (div sequence)
  (let ((pos (position div sequence)))
    (if pos
        (list* (subseq sequence 0 pos)
               (split div (subseq sequence (1+ pos))))
        (list sequence))))

