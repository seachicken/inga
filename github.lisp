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
    (if (and (jsown:keyp json "url"))
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
  (car (last
         (remove-if-not (lambda (body)
                          (string= (first (split #\Newline body)) "# Inga Report"))
                        (mapcar (lambda (c) (jsown:val c "body"))
                                comments)))))

(defun send-pr-comment (hostname base-url owner-repo number affected-poss project-path sha last-report)
  (let ((comment (format nil
                         "~a~%~a~a~a~a~%~%<details><summary>Details</summary>~%~%~a~%</details>"
                         "# Inga Report"
                         "**🔮 "
                         (get-number-of-components affected-poss)
                         " affected by the change**"
                         " (powered by [Inga](https://github.com/seachicken/inga))"
                         (format nil "~{~a~%~}"
                                 (mapcar (lambda (pos) (get-code-url base-url pos sha))
                                         affected-poss)))))
    (when (not (string= comment last-report))
      (uiop:run-program (format nil
                                "(cd ~a && gh pr comment ~a -R ~a/~a --body '~a')"
                                project-path
                                number
                                hostname owner-repo
                                comment)
                        :output :string))))

(defun get-number-of-components (affected-poss)
  (if (equal (length affected-poss) 1)
      "A component"
      (format nil "~a components" (length affected-poss))))

(defun get-code-url (base-url pos sha)
  (let ((path (cdr (assoc :path pos)))
        (line (cdr (assoc :line pos))))
    (format nil
            "~ablob/~a/~a#L~a"
            base-url sha path line)))

(defun split (div sequence)
  (let ((pos (position div sequence)))
    (if pos
        (list* (subseq sequence 0 pos)
               (split div (subseq sequence (1+ pos))))
        (list sequence))))

