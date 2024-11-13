(defpackage #:inga/analyzer/jvm-helper
  (:use #:cl
        #:inga/analyzer/base)
  (:import-from #:inga/config
                #:get-server-config)
  (:export #:get-client-index))
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

