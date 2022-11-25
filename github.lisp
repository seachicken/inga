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
                                          "(cd ~a && gh pr view --json url --json number --json mergeStateStatus --json baseRefName --json commits)"
                                          project-path)
                                  :output :string
                                  :ignore-error-status t))))
    (when (jsown:keyp json "url")
      (append
        (list :base-url
              (get-base-url (jsown:val json "url")))
        (list :owner-repo
              (get-owner-repo (jsown:val json "url")))
        (list :number
              (jsown:val json "number"))
        (list :merge-state-status
              (jsown:val json "mergeStateStatus"))
        (list :base-ref-name
              (jsown:val json "baseRefName"))
        (list :head-sha
              (jsown:val (car (last (jsown:val json "commits"))) "oid"))))))

;; url format: https://HOST/OWNER/REPO/pull/NUMBER
(defun get-base-url (url)
  (let ((paths (split #\/ url)))
    (format nil "~{~a/~}" (subseq paths 0 (- (length paths) 2)))))

(defun get-owner-repo (url)
  (let ((paths (split #\/ url)) result)
    (setf result (format nil "~{~a/~}" (subseq paths 3 5)))
    (subseq result 0 (- (length result) 1))))

(defun send-pr-comment (hostname pr affected-poss project-path)
  (destructuring-bind (&key base-url owner-repo number merge-state-status
                            merge-state-status base-ref-name head-sha) pr
    (let ((entorypoints (group-by-entorypoint affected-poss))
          comment)
      (setf comment
            (format nil
                    "# Inga Report~%**~a affected by the change** (powered by [Inga](https://github.com/seachicken/inga))~%~%<details><summary>Affected files</summary>~%~%~a~%</details>"
                    (get-affected-display-name entorypoints)
                    (get-code-hierarchy base-url head-sha entorypoints)))
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
                                 :output :string))))))

(defun get-affected-display-name (affected-poss)
  (if (equal (length affected-poss) 1)
      "A entrypoint"
      (format nil "~a entrypoints" (length affected-poss))))

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
    with results
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

(defun get-code-hierarchy (base-url sha poss)
  (setf poss (mapcar (lambda (p)
                       (acons :paths (split #\/ (cdr (assoc :path (cdr (assoc :entorypoint p))))) p))
                     poss))
  (let ((sorted-poss (group-by-dir (sort-flat base-url sha poss nil 0)))
        dirs
        (result ""))
    (setf sorted-poss 
          (loop
            with results
            for poss in sorted-poss do
            (setf results
                  (append results
                          (loop
                            with prev
                            with results
                            for pos in poss do
                            (let ((entorypoint (cdr (assoc :entorypoint pos))))
                              (when (and (not (null prev))
                                         (equal entorypoint (cdr (assoc :entorypoint prev))))
                                (setf results (remove prev results)))
                              (setf results (append results (list pos)))
                              (setf prev pos))
                            finally (return (list results)))))
            finally (return results)))
    (setf dirs
          (group-by-short-dirs
            (remove nil
                    (mapcar (lambda (poss)
                              (get-dirs (cdr (assoc :paths (first poss)))))
                            sorted-poss))))
    (loop
      for i from 0 below (length sorted-poss) collect
      (let ((poss (nth i sorted-poss)))
        (multiple-value-bind (dirs-result num-of-nested) (output-dirs dirs i)
          (setf result
                (format nil "~a~a" result dirs-result))
          (loop
            for pos in poss
            do (setf result
                     (format nil "~a~a~%"
                             result
                             (output-file num-of-nested base-url sha pos)))))))
    result))

(defun group-by-short-dirs (dirs-list)
  (loop
    with results
    for sub-i from 0 below (length dirs-list) collect
    (let ((dirs (nth sub-i dirs-list)))
      (setf results 
            (append results
                    (list
                      (loop
                        with tmp-dirs-list = dirs-list
                        with tail-dirs = dirs
                        with sub-results
                        for i from 0 below (length dirs) collect
                        (progn
                          (setf min-i (get-min-matching-index tmp-dirs-list sub-i))
                          (let ((head-dirs (subseq tail-dirs 0 min-i)))
                            (setf tail-dirs (subseq tail-dirs min-i))
                            (setf sub-results
                                  (append sub-results (remove nil (list head-dirs))))
                            (setf tmp-dirs-list (delete-matched-sub-dirs tmp-dirs-list head-dirs))
                            (unless tail-dirs
                              (loop-finish))))
                        finally (progn
                                  (setf sub-results
                                        (append sub-results (remove nil (list tail-dirs))))
                                  (return sub-results)))))))
    finally (return results)))

(defun delete-matched-sub-dirs (dirs-list target-dirs)
  (mapcar (lambda (dirs)
            (let ((subset-size (get-subset-size dirs target-dirs)))
              (if (> subset-size 0)
                (subseq dirs subset-size)
                dirs)))
          dirs-list))

(defun output-dirs (dirs-list output-i)
  (let ((sub-dirs-list (nth output-i dirs-list))
        (head-sub-dirs-list (subseq dirs-list 0 output-i)))
    (loop
      with result = ""
      for i from 0 below (length sub-dirs-list) collect
      (let ((dirs (nth i sub-dirs-list))
            matched-index)
        (setf matched-index (get-matched-index head-sub-dirs-list i dirs))
        (unless matched-index
          (if (= i 0)
              (setf result (format nil "- 📂 ~{~a~^/~}~%" dirs))
              (setf result (format nil "~a~vt- 📂 ~{~a~^/~}~%" result (* i 2) dirs)))))
      finally (return (values result i)))))

(defun get-matched-index (dirs-list num-of-nested target-dirs)
  (loop
    with result
    for i from 0 below (length dirs-list) collect
    (let ((dirs (nth num-of-nested (nth i dirs-list))))
      (when (equal dirs target-dirs)
        (setf result i)
        (loop-finish)))
    finally (return result)))

(defun get-min-matching-index (dirs-list target-i)
  (let ((sub-dirs (subseq dirs-list target-i (length dirs-list)))
        current)
    (setf current (first sub-dirs))
    (unless current
      (return-from get-min-matching-index 0))

    (loop
      with prev-i
      with result = 0
      for i from target-i below (length dirs-list) collect
      (let ((dirs (nth i dirs-list))
            diff-i)
        (setf diff-i (if (equal current dirs)
                         (length current)
                         (get-diff-index current dirs)))
        (when 
          (and (> diff-i 0)
               (or (null prev-i) (< diff-i prev-i)))
          (setf result diff-i))
        (setf prev-i diff-i))
      finally (return result))))

(defun output-file (num-of-nested base-url sha pos)
  (let ((file (get-file (cdr (assoc :paths pos))))
        (origins (cadr (assoc :origins pos)))
        (entorypoint (cdr (assoc :entorypoint pos))))
    (if (= num-of-nested 0)
        (format nil "- 📄 ~a"
                (get-code-url (format nil "~a - ~a"
                                      file (cdr (assoc :name entorypoint)))
                              base-url sha
                              (cdr (assoc :path entorypoint))
                              (cdr (assoc :line entorypoint))))
        (format nil "~vt- 📄 ~a"
                (* num-of-nested 2)
                (get-code-url (format nil "~a - ~a"
                                      file (cdr (assoc :name entorypoint)))
                              base-url sha
                              (cdr (assoc :path entorypoint))
                              (cdr (assoc :line entorypoint)))))))

(defun get-code-url (text base-url sha file-path line)
  (format nil "[~a](~ablob/~a/~a#L~a)"
          text base-url sha file-path line))

(defun group-by-dir (sorted-poss)
  (let (results)
    (loop
      with same-dirs
      for pos in sorted-poss
      do (let ((dirs (get-dirs (cdr (assoc :paths pos))))
               (file (get-file (cdr (assoc :paths pos))))
               (path (cdr (assoc :path pos)))
               (line (cdr (assoc :line pos))))
           (when (and
                   (> (length same-dirs) 0)
                   (not (equal dirs (get-dirs (cdr (assoc :paths (car (last same-dirs))))))))
             (setf results (append results (list same-dirs)))
             (setf same-dirs nil))
           (setf same-dirs (append same-dirs (list pos))))
      finally (setf results (append results (list same-dirs))))
    results))

(defun sort-flat (base-url sha poss results ri)
  (let (files remaining-poss)
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
      with group
      for pos in remaining-poss
      do (let ((entorypoint (cdr (assoc :entorypoint pos)))
               (dir (nth ri (cdr (assoc :paths pos)))))
           (unless prev-dir
             (setf prev-dir dir))

           (when (not (string= dir prev-dir))
             (setf group (sort group #'string< :key
                               #'(lambda (p) (nth (+ ri 1) (cdr (assoc :paths p))))))
             (setf results (sort-flat base-url sha group results (+ ri 1)))
             (setf group nil))
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

(defun get-subset-size (a b)
  (loop
    with result = 0
    for i from 0 below (max (length a) (length b)) collect
    (progn
      (if (and (< i (length a)) (< i (length b)))
          (if (equal (subseq a 0 (+ i 1)) (subseq b 0 (+ i 1)))
              (setf result (+ result 1))
              (loop-finish))))
    finally (return result)))

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

