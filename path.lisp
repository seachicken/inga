(defpackage #:inga/path
  (:use #:cl)
  (:export #:merge-paths
           #:get-variable-names
           #:replace-variable-name))
(in-package #:inga/path)

(defun merge-paths (a b)
  (if (and (uiop:string-suffix-p "/" a) (uiop:string-prefix-p "/" b))
      (concatenate 'string a (subseq b 1))
      (concatenate 'string a b)))

(defun get-variable-names (sequence)
  (loop for c across sequence
        with begin-token
        with variable
        with results
        do
        (alexandria:switch (c)
          (#\{
           (setf begin-token t))
          (#\}
           (setf results (append results (list variable)))
           (setf variable nil) 
           (setf begin-token nil))
          (t
            (when begin-token
              (setf variable (concatenate 'string variable (string c))))))
        finally (return results)))

(defun replace-variable-name (path from to)
  (loop for c across path
        with begin-token
        with variable
        with result
        do
        (alexandria:switch (c)
          (#\{
           (setf begin-token t))
          (#\}
           (setf result (concatenate 'string result "{" (if (equal variable from) to variable) "}"))
           (setf variable nil) 
           (setf begin-token nil))
          (t
            (if begin-token
              (setf variable (concatenate 'string variable (string c)))
              (setf result (concatenate 'string result (string c))))))
        finally (return result)))

