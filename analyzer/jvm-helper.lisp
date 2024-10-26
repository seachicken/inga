(defpackage #:inga/analyzer/jvm-helper
  (:use #:cl
        #:inga/analyzer/base)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path)
  (:export #:get-client-index))
(in-package #:inga/analyzer/jvm-helper)

(defun get-client-index (pos root-path config)
  (let* ((server-base-path (enough-namestring
                             (find-base-path
                               (merge-pathnames
                                 (cdr (assoc :path (cdr (assoc :file-pos pos))))
                                 root-path))
                             root-path))
         (server (when config
                   (find-if (lambda (s)
                              (equal (namestring (pathname (concatenate 'string
                                                                        (cdr (assoc :path s))
                                                                        "/")))
                                     server-base-path))
                            (cdr (assoc :servers config)))))
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

