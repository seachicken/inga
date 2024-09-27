(defpackage #:inga/cli
  (:use #:cl
        #:inga/utils)
  (:import-from #:jsown)
  (:import-from #:inga/analyzer
                #:analyze
                #:start-analyzer
                #:stop-analyzer)
  (:import-from #:inga/ast-index
                #:ast-index-disk)
  (:import-from #:inga/contexts
                #:make-context
                #:context-analyzers
                #:context-lc
                #:context-processes)
  (:import-from #:inga/file
                #:convert-to-pos)
  (:import-from #:inga/git
                #:diff-to-ranges)
  (:import-from #:inga/language-client
                #:make-client
                #:start-client
                #:stop-client)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path)
  (:import-from #:inga/main
                #:run)
  (:import-from #:inga/plugin/jvm-dependency-loader)
  (:import-from #:inga/plugin/spring/spring-property-loader))
(in-package #:inga/cli)

(defmethod run ((mode (eql :cli)) language params)
  (let* ((index (make-instance 'ast-index-disk
                               :root-path (cdr (assoc :root-path params))
                               :temp-path (cdr (assoc :temp-path params))))
         (ctx (case language
                (:typescript
                  (make-context
                    :kind :typescript
                    :project-path (cdr (assoc :root-path params))
                    :include (cdr (assoc :include params))
                    :exclude (cdr (assoc :exclude params))
                    :lc (make-client :typescript (cdr (assoc :root-path params)))
                    :ast-index index
                    :analyzers (list
                                 (start-analyzer :typescript
                                                 (cdr (assoc :include params))
                                                 (cdr (assoc :exclude params))
                                                 (cdr (assoc :root-path params))
                                                 index))))
                (:java
                  (make-context
                    :kind :java
                    :project-path (cdr (assoc :root-path params))
                    :include (cdr (assoc :include params))
                    :exclude (cdr (assoc :exclude params))
                    :ast-index index
                    :analyzers (list
                                 (start-analyzer :java
                                                 (cdr (assoc :include params))
                                                 (cdr (assoc :exclude params))
                                                 (cdr (assoc :root-path params))
                                                 index)
                                 (start-analyzer :kotlin
                                                 (cdr (assoc :include params))
                                                 (cdr (assoc :exclude params))
                                                 (cdr (assoc :root-path params))
                                                 index))
                    :processes (list
                                 (inga/plugin/spring/spring-property-loader:start
                                   (cdr (assoc :root-path params)))
                                 (inga/plugin/jvm-dependency-loader:start
                                   (cdr (assoc :root-path params))))))
                (t (error "context not found. language: ~a" language)))))
    (start-client (context-lc ctx))

    (let ((results (analyze ctx (diff-to-ranges (cdr (assoc :diff params))
                                                (cdr (assoc :root-path params))))))
      (with-open-file (out (merge-pathnames "report.json" (cdr (assoc :output-path params)))
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out "~a" (to-json results (cdr (assoc :root-path params)))))
      (format t "~%~a~%" (to-json results (cdr (assoc :root-path params)))))

    (stop-client (context-lc ctx)) 
    (loop for p in (context-processes ctx) do (uiop:close-streams p)) 
    (loop for a in (context-analyzers ctx) do (stop-analyzer a))))

(defun to-json (results root-path)
  (jsown:to-json
    (mapcan (lambda (r)
              (let ((obj
                      `((:obj
                          ("type" . ,(cdr (assoc :type r)))
                          ("origin" . ,(cons :obj (key-downcase
                                                    (convert-to-output-pos
                                                      root-path
                                                      (cdr (assoc :origin r))))))
                          ("entrypoint" . ,(cons :obj (key-downcase
                                                        (convert-to-output-pos
                                                          root-path
                                                          (cdr (assoc :entrypoint r))))))
                          ,@(when (equal (cdr (assoc :type r)) "entrypoint")
                              `(("service" . ,(first (last
                                                       (pathname-directory
                                                         (find-base-path
                                                           (merge-pathnames
                                                             (cdr (assoc :path
                                                                         (convert-to-output-pos
                                                                           root-path
                                                                           (cdr (assoc :entrypoint r)))))
                                                             root-path))))))))))))
                obj))
            results)))

(defun convert-to-output-pos (root-path pos)
  (when (eq (cdr (assoc :type pos)) :rest-server)
    (setf pos (cdr (assoc :file-pos pos))))
  (let ((text-pos (convert-to-pos (merge-pathnames (cdr (assoc :path pos)) root-path)
                                  (cdr (assoc :top-offset pos)))))
    (list
      (cons :path (enough-namestring (cdr (assoc :path pos)) root-path))
      (cons :name (cdr (assoc :name pos)))
      (cons :line (cdr (assoc :line text-pos)))
      (cons :offset (cdr (assoc :offset text-pos))))))

(defun key-downcase (obj)
  (mapcar (lambda (p) (cons (string-downcase (car p)) (cdr p))) obj))

