(defpackage #:inga/main
  (:use #:cl
        #:inga/git
        #:inga/ts-helper
        #:inga/jsx)
  (:import-from #:jsown)
  (:export #:start
           #:stop
           #:analyze
           #:find-components
           #:inject-mark))
(in-package #:inga/main)

(defvar *deepest-item*)
(defvar *tsserver*)
(defvar *tsparser*)

(defun start ()
  (setq *tsserver* (uiop:launch-program "tsserver" :input :stream :output :stream)))

(defun stop ()
  (uiop:close-streams *tsserver*))

(defun start-tsparser ()
  (setq *tsparser* (uiop:launch-program "tsparser" :input :stream :output :stream)))

(defun stop-tsparser ()
  (uiop:close-streams *tsparser*))

(defun get-val (list key)
  (loop for item in list do
        (when (equal (car item) key)
          (return-from get-val (cdr item)))))

(defun analyze (project-path sha-a &optional (sha-b))
  (remove-duplicates
    (mapcan (lambda (range)
              (let ((src-path (format nil "~a~a" project-path (get-val range "path"))))
                (call (format nil "{\"seq\": 1, \"command\": \"open\", \"arguments\": {\"file\": \"~a\"}}" src-path))

                (defvar result)
                (setq result (exec (format nil "{\"seq\": 2, \"command\": \"navtree\", \"arguments\": {\"file\": \"~a\"}}" src-path)))
                (mapcan (lambda (pos)
                          (find-components project-path src-path pos))
                        (get-affected-item-poss range))
                  ))
            (get-diff project-path sha-a sha-b))
    :test #'equal))

(defun get-affected-item-poss (range)
  (remove-duplicates
    (remove nil
            (mapcar (lambda (line)
                      (find-affected-item-pos line))
                    (loop for line
                          from (get-val range "start")
                          to (get-val range "end")
                          collect line)))
    :test #'equal))

(defun find-affected-item-pos (line)
  (setq *deepest-item* nil)
  (find-item (jsown:val result "body") line)
  (get-pos *deepest-item*))

(defun find-item (tree line)
  (when (contains-line tree line)
    (setq *deepest-item* tree))

  (if (jsown:keyp tree "childItems")
      (progn
        (find-item (jsown:val tree "childItems") line))
      (when (jsown:keyp tree "OBJ")
        (loop for obj in tree do
              (find-item obj line)))))

(defun find-components (root-path src-path pos)
  (call (format nil "{\"seq\": 1, \"command\": \"open\", \"arguments\": {\"file\": \"~a\"}}" src-path))
  (let ((result (exec (format nil "{\"seq\": 2, \"command\": \"references\", \"arguments\": {\"file\": \"~a\", \"line\": ~a, \"offset\": ~a}}"
                              src-path (jsown:val pos "line") (jsown:val pos "offset"))))
        poss)
    (let ((refposs
          (remove-if (lambda (p) (equal p pos))
                     (mapcar (lambda (r)
                               (list
                                 (cons :path (jsown:val r "file"))
                                 (cons :line (jsown:val (jsown:val r "start") "line"))
                                 (cons :offset (jsown:val (jsown:val r "start") "offset"))))
                             (jsown:val (jsown:val result "body") "refs")))))
      (setq poss (mapcar (lambda (p)
                           (convert-to-ast-pos p))
                         refposs)))

    (start-tsparser)
    (setq poss
          (remove nil
                  (mapcar
                    (lambda (p)
                      (let ((result (exec-tsparser (namestring (cdr (assoc :path p))))))
                        (when (> (length result) 0)
                          (acons :ast (cdr (jsown:parse result)) p))))
                    poss)))
    (stop-tsparser)

    (remove-duplicates
      (remove nil
              (mapcar (lambda (p)
                        (let ((path (cdr (assoc :path p)))
                              (pos (cdr (assoc :pos p)))
                              (ast (cdr (assoc :ast p))))
                          (setq *nearest-comp-pos* nil)
                          (let ((comp-pos (find-component ast pos)))
                            (when (not (null comp-pos))
                              (convert-to-pos root-path path comp-pos)))))
                      poss))
      :test #'equal)))

(defparameter *jsx-opening-element* 276)
(defparameter *jsx-self-closing-element* 275)

(defparameter *nearest-comp-pos* nil)

(defun find-component (ast pos)
  (when (and (jsown:keyp ast "kind")
             (or
               (equal (jsown:val ast "kind") *jsx-opening-element*) 
               (equal (jsown:val ast "kind") *jsx-self-closing-element*)))
    (setq *nearest-comp-pos* (+ (jsown:val ast "start") 1)))

  (when (not (null ast))
    (if (consp (car ast))
        (progn
          (when (and (jsown:keyp ast "start") (equal (jsown:val ast "start") pos))
            (return-from find-component *nearest-comp-pos*))

        (if (consp (cdr (car ast)))
            (progn
              (let ((comp-pos (find-component (cdr (car ast)) pos)))
                (when (not (null comp-pos))
                  (return-from find-component comp-pos)))
              (return-from find-component (find-component (cdr ast) pos)))
            (progn
              (when (cdr ast)
                (return-from find-component (find-component (cdr ast) pos))))))
      (progn
        (return-from find-component (find-component (cdr ast) pos))))))

(defun find-child-nodes (node)
  (if (jsown:keyp node "statements")
    (jsown:val node "statements")
  (when (jsown:keyp node "parameters")
    (jsown:val node "parameters"))))

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

(defun exec (command)
  (call command)
  (let ((stream (uiop:process-info-output *tsserver*)))
    (loop while stream do
          (defvar result)
          (setq result (jsown:parse (extjson stream)))
          (when (string= (jsown:val result "type") "response")
            (return result)))))

(defun exec-tsparser (command)
  (call-tsparser command)
  (let ((stream (uiop:process-info-output *tsparser*)))
    (read-line stream)))

(defun call (command)
  (write-line command (uiop:process-info-input *tsserver*))
  (force-output (uiop:process-info-input *tsserver*)))

(defun call-tsparser (command)
  (write-line command (uiop:process-info-input *tsparser*))
  (force-output (uiop:process-info-input *tsparser*)))

(defun extjson (stream)
  ;; Content-Length: 99
  (read-line stream)
  ;; newline
  (read-line stream)
  ;; JSON 
  (read-line stream))
