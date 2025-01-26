(defpackage #:inga/cli
  (:use #:cl
        #:inga/utils)
  (:import-from #:alexandria)
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
  (:import-from #:inga/git
                #:diff-to-ranges)
  (:import-from #:inga/language-client
                #:make-client
                #:start-client
                #:stop-client)
  (:import-from #:inga/main
                #:run)
  (:import-from #:inga/plugin/jvm-dependency-loader)
  (:import-from #:inga/plugin/spring/spring-property-loader)
  (:import-from #:inga/reporter
                #:output-error
                #:output-report))
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
                  (let ((processes (list
                                     (inga/plugin/spring/spring-property-loader:start
                                       (cdr (assoc :root-path params)))
                                     (inga/plugin/jvm-dependency-loader:start
                                       (cdr (assoc :root-path params))))))
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
                      :processes processes)))
                (t (error "context not found. language: ~a" language)))))
    (start-client (context-lc ctx))

    (let* (failures
           (results (analyze ctx (diff-to-ranges (cdr (assoc :diff params))
                                                 (cdr (assoc :root-path params)))
                             :failure (lambda (fs) (setf failures fs))
                             :config (cdr (assoc :config params)))))
      (output-error failures
                    (cdr (assoc :output-path params))
                    (cdr (assoc :root-path params)))
      (format t "~%~a~%"
              (alexandria:read-file-into-string
                (output-report results
                               (cdr (assoc :output-path params))
                               (cdr (assoc :root-path params))))))

    (stop-client (context-lc ctx)) 
    (loop for p in (context-processes ctx) do (uiop:close-streams p)) 
    (loop for a in (context-analyzers ctx) do (stop-analyzer a))))

