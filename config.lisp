(defpackage #:inga/config
  (:use #:cl)
  (:import-from #:jsown)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path)
  (:export #:get-server-config
           #:config-has-changed
           #:config-to-obj
           #:obj-to-config
           #:parse-yaml
           #:to-yaml))
(in-package #:inga/config)

(defun get-server-config (config pos root-path)
  (when (or (null config) (not (eq (cdr (assoc :type pos)) :rest-server)))
    (return-from get-server-config))

  (let ((module-path (enough-namestring
                       (find-base-path
                         (cdr (assoc :path (cdr (assoc :file-pos pos))))
                         root-path)
                       root-path)))
    (find-if (lambda (s)
               (equal (namestring (pathname (concatenate 'string
                                                         (cdr (assoc :path s))
                                                         "/")))
                      module-path))
             (cdr (assoc :servers config)))))

(defun config-has-changed (a b pos root-path)
  (not (equal (get-server-config a pos root-path)
              (get-server-config b pos root-path))))

(defun config-to-obj (config)
  (labels ((convert-servers (servers)
             (mapcar (lambda (s)
                       `(:obj
                          ("path" . ,(cdr (assoc :path s)))
                          ,@(when (assoc :clients s)
                              `(("clients" .
                                 ,(convert-clients (cdr (assoc :clients s))))))))
                     servers))
           (convert-clients (clients)
             (mapcar (lambda (c)
                       `(:obj
                          ("path" . ,(cdr (assoc :path c)))))
                     clients)))
    `(:obj
       ("servers" . ,(convert-servers (cdr (assoc :servers config)))))))

(defun obj-to-config (obj)
  (labels ((convert-servers (servers)
             (mapcar (lambda (s) `((:path . ,(jsown:val s "path"))
                                   ,@(when (jsown:keyp s "clients")
                                       `((:clients .
                                          ,(convert-clients (jsown:val s "clients")))))))
                     servers))
           (convert-clients (clients)
             (mapcar (lambda (c) `((:path . ,(jsown:val c "path"))))
                     clients)))
    (normalize-config
      `((:servers . ,(convert-servers (jsown:val obj "servers")))))))

(defun parse-yaml (value)
  (with-input-from-string (in value)
    (loop for line = (read-line in nil nil)
          with stack
          with prev-indent = -1
          with next-has-list
          while line
          do
          (let* ((trim-line (string-trim '(#\Space) line))
                 (indent (when (and (>= (length trim-line) 2)
                                    (equal (subseq trim-line 0 2) "- "))
                           (count #\Space (string-right-trim '(#\Space) line))))
                 (indent-diff (if (or (null indent) (< prev-indent 0))
                                  0 (- indent prev-indent))))
            (when next-has-list
              (when (> indent-diff 0)
                (push (caar (last (cdar stack))) stack)
                (setf next-has-list nil)))
            (when (< indent-diff 0) (pop stack))

            (when (uiop:string-prefix-p "- path:" trim-line)
              (setf (cdr (first stack))
                    (append (cdr (first stack))
                            (list `((:path . ,(string-trim '(#\Space) (subseq trim-line 7))))))))
            (when (uiop:string-prefix-p "clients:" trim-line)
              (setf (car (last (cdar stack)))
                    (acons :clients nil (car (last (cdar stack)))))
              (setf next-has-list t))
            (when (equal trim-line "servers:")
              (setf stack (acons :servers nil stack)))

            (when indent
              (setf prev-indent indent)))
          finally (return (normalize-config (list (car (last stack))))))))

(defun to-yaml (config)
  (loop for server in (cdr (assoc :servers config))
        with result = (concatenate 'string "servers:" '(#\Newline))
        do
        (setf result (concatenate 'string result "  - path: "
                                  (cdr (assoc :path server)) '(#\Newline)))
        (when (and (assoc :clients server) (>= (length (cdr (assoc :clients server))) 1))
          (setf result (concatenate 'string result "    clients:" '(#\Newline)))
          (loop for client in (cdr (assoc :clients server))
                do
                (setf result (concatenate 'string result "      - path: "
                                          (cdr (assoc :path client)) '(#\Newline)))))
        finally (return result)))

(defun normalize-config (config)
  (labels ((sort-by-path (values)
             (sort values #'string< :key (lambda (v) (cdr (assoc :path v))))))
    `((:servers .
        ,(mapcar (lambda (s)
                   (if (assoc :clients s)
                       (progn
                         (setf (cdr (assoc :clients s)) (sort-by-path (cdr (assoc :clients s))))
                         s)
                       (acons :clients (sort-by-path (when (assoc :clients s)
                                                       (cdr (assoc :clients s))))
                              s)))
                 (sort-by-path (cdr (assoc :servers config))))))))

