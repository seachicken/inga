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
                                          "(cd ~a && gh pr view --json url --json number --json baseRefName --json commits)"
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
              (jsown:val (car (last (jsown:val json "commits"))) "oid")))
      '())))

;; url format: https://HOST/OWNER/REPO/pull/NUMBER
(defun get-base-url (url)
  (let ((paths (split #\/ url)))
    (format nil "~{~a/~}" (subseq paths 0 (- (length paths) 2)))))

(defun get-owner-repo (url)
  (let ((paths (split #\/ url)) result)
    (setf result (format nil "~{~a/~}" (subseq paths 3 5)))
    (subseq result 0 (- (length result) 1))))

(defun send-pr-comment (hostname base-url owner-repo number affected-poss project-path sha min-combination)
  (let ((combinations (filter-combinations
                        (filter-by-key (sort-by-combination affected-poss) :origin)
                        min-combination))
        (entorypoints (group-by-entorypoint affected-poss))
        comment)
    (setf comment
          (format nil
                  "# Inga Report~%**~a affected by the change** (powered by [Inga](https://github.com/seachicken/inga))~%~%<details><summary>Affected files</summary>~%~%~a~a~%</details>"
                  (get-affected-display-name entorypoints)
                  (if (> (length combinations) 0)
                      (format nil "Change with the highest number of combinations:~%~%~a~%" (get-combination-table base-url sha combinations))
                      "")
                  (get-code-hierarchy base-url sha entorypoints combinations)))
    (handler-case
      (uiop:run-program (format nil
                                "(cd ~a && gh pr comment ~a -R ~a/~a --body '~a' --edit-last)"
                                project-path
                                number
                                hostname owner-repo
                                comment)
                        :output :string)
      (uiop:subprocess-error ()
                             (uiop:run-program
                               (format nil
                                       "(cd ~a && gh pr comment ~a -R ~a/~a --body '~a')"
                                       project-path
                                       number
                                       hostname owner-repo
                                       comment)
                               :output :string)))))

(defun get-affected-display-name (affected-poss)
  (if (equal (length affected-poss) 1)
      "A entrypoint"
      (format nil "~a entrypoints" (length affected-poss))))

(defun sort-by-combination (poss)
  (sort (copy-list poss)
        #'(lambda (a b) (> (cdr (assoc :combination (cdr (assoc :origin a))))
                           (cdr (assoc :combination (cdr (assoc :origin b))))))))

(defun group-by-entorypoint (poss)
  (setf poss (mapcar (lambda (p)
                       (acons :paths (split #\/ (cdr (assoc :path (cdr (assoc :entorypoint p))))) p))
                     poss))
  (setf sorted-poss (sort poss #'string< :key
                          #'(lambda (p)
                              (format nil "~a~a"
                                      (cdr (assoc :paths p))
                                      (cdr (assoc :line (cdr (assoc :entorypoint p))))))))
  (loop
    with prev
    with results = '()
    for pos in sorted-poss do
    (let ((entorypoint-current (cdr (assoc :entorypoint pos)))
          (entorypoint-prev (cdr (assoc :entorypoint prev))))
      (if (equal entorypoint-current entorypoint-prev)
          (progn
            (setf (cadr (assoc :origins (car (last results))))
                  (append (cadr (assoc :origins (car (last results))))
                          (list (cdr (assoc :origin pos))))))
          (setf results (append results
                                (list
                                  (list
                                    (cons :entorypoint (cdr (assoc :entorypoint pos)))
                                    (cons :origins (list (list (cdr (assoc :origin pos))))))))))
      (setf prev pos))
    finally (return results)))

(defun filter-by-key (poss key)
  (loop
    with prev
    with results = '()
    for pos in poss do
    (let ((val-current (cdr (assoc key pos)))
          (val-prev (cdr (assoc key prev))))
      (when (not (equal val-current val-prev))
        (setf results (append results (list pos))))
      (setf prev pos))
    finally (return results)))

(defun filter-combinations (poss min-combination)
  (loop
    with rank = 1
    with prev-combination = 0
    with results = '()
    for pos in poss do
    (let ((origin (cdr (assoc :origin pos)))
          combination)
      (setf combination (cdr (assoc :combination origin)))
      (when (and
              (>= combination min-combination)
              (<= rank 3) (>= (length poss) rank))
        (when (< combination prev-combination)
          (setf rank (+ rank 1)))
        (setf prev-combination combination)
        (setf pos (acons :rank rank pos))
        (setf results (append results (list pos)))))
    finally (return results)))

(defun get-combination-table (base-url sha poss)
  (let ((result ""))
    (format nil "~a~%~a~%~a"
            "| Rank | Changed Functions | Combinations |"
            "| - | - | - |"
            (loop
              with result = ""
              for pos in poss do
              (let ((origin (cdr (assoc :origin pos)))
                    (rank (cdr (assoc :rank pos)))
                    origin-link)
                (setf origin-link 
                      (get-code-url (format nil "~a - ~a"
                                            (get-file (split #\/ (cdr (assoc :path origin))))
                                            (cdr (assoc :name origin)))
                                    base-url sha
                                    (cdr (assoc :path origin))
                                    (cdr (assoc :line origin))))
                (setf result (format nil "~a| ~a | ~a | ~a~a |~%"
                                     result
                                     rank
                                     origin-link
                                     (cdr (assoc :combination origin))
                                     (if (= rank 1) " 💥" ""))))
              finally (return result)))))

(defun get-code-hierarchy (base-url sha poss combinations)
  (setf poss (mapcar (lambda (p)
                       (acons :paths (split #\/ (cdr (assoc :path (cdr (assoc :entorypoint p))))) p))
                     poss))
  (let ((sorted-poss (group-by-dir (sort-flat base-url sha poss '() 0)))
        (most-affected-poss (remove-if-not (lambda (p)
                                             (= (cdr (assoc :rank p)) 1))
                                           combinations))
        (result ""))
    (setf sorted-poss 
          (loop
            with results = '()
            for poss in sorted-poss do
            (setf results
                  (append results
                          (loop
                            with prev
                            with results = '()
                            for pos in poss do
                            (let ((entorypoint (cdr (assoc :entorypoint pos))))
                              (when (and (not (null prev))
                                         (equal entorypoint (cdr (assoc :entorypoint prev))))
                                (setf results (remove prev results)))
                              (setf results (append results (list pos)))
                              (setf prev pos))
                            finally (return (list results)))))
            finally (return results)))
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
               do (setf result
                        (format nil "~a~a~%"
                                result
                                (output-file num-of-nested base-url sha pos most-affected-poss)))))
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

(defun output-file (num-of-nested base-url sha pos most-affected-poss)
  (let ((file (get-file (cdr (assoc :paths pos))))
        (origins (cadr (assoc :origins pos)))
        (most-affected-origins (mapcar (lambda (p) (cdr (assoc :origin p))) most-affected-poss))
        (entorypoint (cdr (assoc :entorypoint pos)))
        (explosion ""))
    (loop for origin in origins do
          (when (find origin most-affected-origins :test #'equal)
            (setf explosion " 💥")))
    (if (= num-of-nested 0)
        (format nil "- 📄 ~a ⇠ ~a"
                (get-code-url (format nil "~a - ~a~a"
                                      file (cdr (assoc :name entorypoint)) explosion)
                              base-url sha
                              (cdr (assoc :path entorypoint))
                              (cdr (assoc :line entorypoint)))
                (get-origin-links base-url sha pos))
        (format nil "~vt- 📄 ~a ⇠ ~a"
                (* num-of-nested 2)
                (get-code-url (format nil "~a - ~a~a"
                                      file (cdr (assoc :name entorypoint)) explosion)
                              base-url sha
                              (cdr (assoc :path entorypoint))
                              (cdr (assoc :line entorypoint)))
                (get-origin-links base-url sha pos)))))

(defun get-code-url (text base-url sha file-path line)
  (format nil "[~a](~ablob/~a/~a#L~a)"
          text base-url sha file-path line))

(defun get-origin-links (base-url sha pos)
  (loop
    with results = ""
    for origin in (cadr (assoc :origins pos)) do
    (setf results (format nil "~a~a~a"
                          results
                          (if (= (length results) 0) "" " ")
                          (get-code-url "✶" base-url sha
                                        (cdr (assoc :path origin))
                                        (cdr (assoc :line origin)))))
    finally (return results)))

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
                      #'(lambda (f)
                          (let ((entorypoint (cdr (assoc :entorypoint f))))
                            (format nil "~a~a"
                                     (nth ri (cdr (assoc :paths f)))
                                     (cdr (assoc :line entorypoint)))))))
    (setf results (append files results))

    (when (= (length remaining-poss) 0)
      (return-from sort-flat results))

    ;; sort by same directory
    (loop
      with prev-dir
      with group = '()
      for pos in remaining-poss
      do (let ((entorypoint (cdr (assoc :entorypoint pos)))
               (dir))
           (setf dir (nth ri (cdr (assoc :paths pos))))
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

