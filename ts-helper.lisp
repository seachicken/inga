(defpackage #:inga/ts-helper
  (:use #:cl)
  (:import-from #:jsown)
  (:export #:get-pos
           #:contains-line
           #:convert-to-ast-pos
           #:convert-to-pos))
(in-package #:inga/ts-helper)

(defun get-pos (item)
  (when (jsown:keyp item "nameSpan")
    (let ((name-span (jsown:val item "nameSpan")) start)
      (setq start (jsown:val name-span "start"))
      (cons :pos (cdr start)))))

(defun contains-line (tree line)
  (when (and (jsown:keyp tree "text") (not (string= (jsown:val tree "kind") "module")))
    (let ((span (car (jsown:val tree "spans"))) start end)
      (setq start (jsown:val (jsown:val span "start") "line"))
      (setq end (jsown:val (jsown:val span "end") "line"))
      (and (>= line start) (>= end line)))))

(defun convert-to-ast-pos (pos)
  (let ((path (cdr (assoc :path pos)))
        (line-no 0)
        (result 0))
    (with-open-file (stream path)
      (loop for line = (read-line stream nil)
            while line
            when (= line-no (- (cdr (assoc :line pos)) 1))
            return (list
                     (cons :path (pathname path))
                     (cons :pos (- (+ result (cdr (assoc :offset pos))) 1)))
            do
            (setq line-no (+ line-no 1))
            ;; add newline code
            (setq result (+ result (+ (length line) 1)))))))

(defun convert-to-pos (root-path path pos)
  (let ((line-no 0)
        (cnt 0))
    (with-open-file (stream path)
      (loop for line = (read-line stream nil)
            while line
            when (<= pos (+ cnt (length line) 1))
            return (list
                     (cons :path (enough-namestring (namestring path) root-path))
                     (cons :line (+ line-no 1))
                     (cons :offset (- (+ (length line) 1) (- (+ cnt (length line)) pos))))
            do
              (setq line-no (+ line-no 1))
              ;; add newline code
              (setq cnt (+ cnt (length line) 1))))))
