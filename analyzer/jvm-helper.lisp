(defpackage #:inga/analyzer/jvm-helper
  (:use #:cl
        #:inga/analyzer/base)
  (:import-from #:inga/config
                #:get-server-config)
  (:export #:get-client-index
           #:get-fq-class-name-candidates))
(in-package #:inga/analyzer/jvm-helper)

(defun get-client-index (pos root-path config)
  (let* ((server (get-server-config config pos root-path))
         (client-base-paths (mapcar (lambda (c)
                                      (namestring
                                        (pathname
                                          (concatenate
                                            'string
                                            (namestring (merge-pathnames (cdr (assoc :path c))
                                                                         root-path))
                                            "/"))))
                                    (when server (cdr (assoc :clients server))))))
    (if server
        (mapcan (lambda (c) (gethash (intern (namestring c) :keyword)
                                     (gethash :module *file-index*)))
                client-base-paths)
        (gethash :rest-client *file-index*))))

(defun get-fq-class-name-candidates (class-name imports)
  (loop for import in imports
        with results
        do
        (when (uiop:string-suffix-p import (concatenate 'string "." class-name))
          (return (values (list import) :exact)))
        (when (uiop:string-suffix-p import ".*")
          (push (concatenate 'string (subseq import 0 (1- (length import))) class-name) results))
        finally (return (values (reverse results) :fuzzy))))

