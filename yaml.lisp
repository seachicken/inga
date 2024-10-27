(defpackage #:inga/yaml
  (:use #:cl)
  (:export #:parse-yaml))
(in-package #:inga/yaml)

(defun parse-yaml (value)
  (with-input-from-string (in value)
    (loop for line = (read-line in nil nil)
          with stack
          with prev-indent = -1
          while line
          do
          (let* ((trim-line (string-trim '(#\Space) line))
                 (indent (when (and (>= (length trim-line) 2)
                                    (equal (subseq trim-line 0 2) "- "))
                           (count #\Space (string-right-trim '(#\Space) line))))
                 (indent-diff (if (or (null indent) (< prev-indent 0))
                                  0 (- indent prev-indent))))
            (when (< indent-diff 0) (pop stack))

            (when (uiop:string-prefix-p "- path:" trim-line)
              (setf (cdr (first stack))
                    (append (cdr (first stack))
                            (list `((:path . ,(string-trim '(#\Space) (subseq trim-line 7))))))))
            (when (uiop:string-prefix-p "clients:" trim-line)
              (setf (car (last (cdar stack)))
                    (acons :clients nil (car (last (cdar stack)))))
              (push (caar (last (cdar stack))) stack))
            (when (equal trim-line "servers:")
              (setf stack (acons :servers nil stack)))

            (when indent
              (setf prev-indent indent)))
          finally (return (list (car (last stack)))))))

