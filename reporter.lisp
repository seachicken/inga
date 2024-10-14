(defpackage #:inga/reporter
  (:use #:cl)
  (:import-from #:jsown)
  (:import-from #:inga/analyzer/base
                #:signature-load-failed
                #:signature-load-failed-fq-class-name
                #:signature-load-failed-path)
  (:import-from #:inga/file
                #:convert-to-pos)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path)
  (:export #:output-report
           #:output-error
           #:convert-to-report-pos))
(in-package #:inga/reporter)

(defun output-report (results output-path root-path)
  (with-open-file (out (merge-pathnames "report.json" output-path)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (labels ((convert (poss)
               (mapcan (lambda (pos)
                         `((:obj
                             ("type" . ,(cdr (assoc :type pos)))
                             ("origin" . ,(cons :obj (convert-to-report-pos
                                                       (cdr (assoc :origin pos))
                                                       root-path)))
                             ,@(when (assoc :entrypoint pos)
                                 `(("entrypoint" . ,(cons :obj (convert-to-report-pos
                                                                 (cdr (assoc :entrypoint pos))
                                                                 root-path)))))
                             ,@(when (or (equal (cdr (assoc :type pos)) "entrypoint")
                                         (equal (cdr (assoc :type pos)) "searching"))
                                 `(("service" . ,(find-service-name
                                                   (cdr (assoc "path"
                                                               (convert-to-report-pos
                                                                 (if (equal (cdr (assoc :type pos))
                                                                            "entrypoint")
                                                                     (cdr (assoc :entrypoint pos))
                                                                     (cdr (assoc :origin pos)))
                                                                 root-path)))
                                                   root-path)))))))
                       poss)))
      (format out "~a"
              (jsown:to-json
                `(:obj
                   ("version" . "0.2")
                   ("results" . ,(mapcar #'convert results)))))))
  (merge-pathnames "report.json" output-path))

(defun output-error (errors output-path root-path)
  (with-open-file (out (merge-pathnames "error.json" output-path)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (labels ((get-type (error)
               (string-downcase (type-of error)))
             (convert (errors)
               (loop for error in errors
                     with results
                     do
                     (typecase error
                       (signature-load-failed
                         (push `(:obj
                                  ("type" . ,(get-type error))
                                  ("service". ,(find-service-name (signature-load-failed-path error)
                                                                  root-path))
                                  ("path" . ,(signature-load-failed-path error))
                                  ("fq-class-name" . ,(signature-load-failed-fq-class-name error)))
                               results)))
                     finally (return (remove-duplicates results :test #'equal)))))
      (format out "~a"
              (jsown:to-json
                `(:obj
                   ("version" . "0.1")
                   ("errors" . ,(convert errors)))))))
  (merge-pathnames "error.json" output-path))

(defun convert-to-report-pos (pos root-path)
  (unless pos (return-from convert-to-report-pos))

  (when (eq (cdr (assoc :type pos)) :rest-server)
    (setf pos (cdr (assoc :file-pos pos))))
  (let ((text-pos (convert-to-pos (merge-pathnames (cdr (assoc :path pos)) root-path)
                                  (cdr (assoc :top-offset pos)))))
    `(("path" . ,(enough-namestring (cdr (assoc :path pos)) root-path))
      ("name" . ,(cdr (assoc :name pos)))
      ("line" . ,(cdr (assoc :line text-pos)))
      ("offset" . ,(cdr (assoc :offset text-pos))))))

(defun find-service-name (path root-path)
  (first
    (last
      (pathname-directory
        (find-base-path (merge-pathnames path root-path))))))

