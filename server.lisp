(defpackage #:inga/server
  (:use #:cl
        #:inga/utils)
  (:import-from #:flexi-streams)
  (:import-from #:jsown)
  (:import-from #:local-time)
  (:import-from #:inga/analyzer
                #:analyze
                #:find-definitions 
                #:get-module-paths
                #:start-analyzer
                #:stop-analyzer)
  (:import-from #:inga/ast-index
                #:ast-index-disk
                #:update-index)
  (:import-from #:inga/config
                #:config-to-obj
                #:obj-to-config
                #:to-yaml)
  (:import-from #:inga/contexts
                #:make-context
                #:context-analyzers
                #:context-ast-index
                #:context-exclude
                #:context-include
                #:context-kind
                #:context-lc
                #:context-processes)
  (:import-from #:inga/file
                #:convert-to-top-offset
                #:is-analysis-target)
  (:import-from #:inga/git
                #:diff-to-ranges)
  (:import-from #:inga/logger
                #:log-debug
                #:log-debug-generic
                #:log-error
                #:log-error-generic
                #:log-info-generic)
  (:import-from #:inga/main
                #:run)
  (:import-from #:inga/reporter
                #:convert-to-report-pos
                #:output-error
                #:output-report))
(in-package #:inga/server)

(defparameter *indexing-token* "indexing-progress")
(defparameter *msg-q* nil)
(defparameter *processing-msg* nil)

(defmethod run ((mode (eql :server)) language params)
  (handle-msg params))

(defun handle-msg (params &optional ctx client-params)
  (let ((root-path (cdr (assoc :root-path params)))
        (output-path (cdr (assoc :output-path params)))
        (temp-path (cdr (assoc :temp-path params)))
        (base-commit (cdr (assoc :base-commit params)))
        (config (cdr (assoc :config params)))
        (msg (loop while *standard-input* do
                   (let* ((json (extract-json *standard-input*))
                          (result (when json (jsown:parse json))))
                     (return result)))))
    (when msg (enqueue-msg msg))
    (setf msg (dequeue-msg))
    (when msg
      (cond
        ((equal (jsown:val msg "method") "initialize")
         (log-debug "run initialize processing")
         (output-report `(()) output-path root-path)
         (let* ((index (make-instance 'ast-index-disk :root-path root-path :temp-path temp-path))
                (capabilities (jsown:val (jsown:val msg "params") "capabilities"))
                (work-done-progress
                  (when (and (jsown:keyp capabilities "window")
                             (jsown:keyp (jsown:val capabilities "window") "workDoneProgress"))
                    (jsown:val (jsown:val capabilities "window") "workDoneProgress")))
                root-host-paths)
           (labels ((print-begin-indexing ()
                      (when work-done-progress
                        (print-notification-msg
                          "window/workDoneProgress/create"
                          (jsown:to-json
                            `((:obj
                                ("token" . ,*indexing-token*)))))
                        (print-notification-msg
                          "$/progress"
                          (jsown:to-json
                            `((:obj
                                ("token" . ,*indexing-token*)
                                ("value" .
                                 (:obj
                                   ("kind" . "begin")
                                   ("title" . "Indexing")))))))))
                    (print-end-indexing ()
                      (when work-done-progress
                        (print-notification-msg
                          "$/progress"
                          (jsown:to-json
                            `((:obj
                                ("token" . ,*indexing-token*)
                                ("value" .
                                 (:obj
                                   ("kind" . "end")))))))))
                    (print-report-indexing (p)
                      (when work-done-progress
                        (print-notification-msg
                          "$/progress"
                          (jsown:to-json
                            `((:obj
                                ("token" . ,*indexing-token*)
                                ("value" .
                                 (:obj
                                   ("kind" . "report")
                                   ("message" . ,(format nil "Update indexes~%~a"
                                                         (progress-path p))))))))))))
             (setf ctx
                   (case language
                     (t
                       (let (analyzers)
                         (print-begin-indexing)
                         (handler-bind ((progress #'print-report-indexing))
                           (loop for lang in '(:java :kotlin)
                                 do
                                 (push (start-analyzer lang
                                                       (cdr (assoc :include params))
                                                       (cdr (assoc :exclude params))
                                                       root-path
                                                       index)
                                       analyzers)))
                         (print-end-indexing)

                         (make-context
                           :kind :java
                           :project-path (cdr (assoc :root-path params))
                           :include (cdr (assoc :include params))
                           :exclude (cdr (assoc :exclude params))
                           :ast-index index
                           :analyzers analyzers
                           :processes (list
                                        (inga/plugin/spring/spring-property-loader:start root-path)
                                        (inga/plugin/jvm-dependency-loader:start root-path))))))))
           (when (jsown:val (jsown:val msg "params") "rootUri")
             (push (namestring (pathname
                                 (concatenate
                                   'string
                                   ;; remove URI scheme (file://)
                                   (subseq (jsown:val (jsown:val msg "params") "rootUri") 7)
                                   "/")))
                   root-host-paths))
           (when (jsown:keyp (jsown:val msg "params") "workspaceFolders")
             (loop for folder in (jsown:val (jsown:val msg "params") "workspaceFolders")
                   do
                   (push (namestring (pathname
                                       (concatenate
                                         'string
                                         (subseq (jsown:val folder "uri") 7)
                                         "/")))
                         root-host-paths)))
           (setf client-params
                 `((:root-host-paths . ,root-host-paths)
                   (:work-done-progress . ,work-done-progress)))
           (print-response-msg (jsown:val msg "id") "{\"capabilities\":{\"textDocumentSync\":{\"change\":2,\"save\":false}}}")))
        ((equal (jsown:val msg "method") "shutdown")
         (log-debug "run shutdown processing")

         (loop for p in (context-processes ctx) do (uiop:close-streams p)) 
         (loop for a in (context-analyzers ctx) do (stop-analyzer a))

         (print-response-msg (jsown:val msg "id") "null")
         (return-from handle-msg))
        ((equal (jsown:val msg "method") "workspace/executeCommand")
         (let* ((params (jsown:val msg "params"))
                (command (jsown:val params "command"))
                (arguments (when (jsown:keyp params "arguments") (jsown:val params "arguments"))))
           (log-debug (format nil "run workspace/executeCommand (command: ~a) processing" command))
           (cond
             ((equal command "inga.getModulePaths")
              (print-response-msg
                (jsown:val msg "id")
                (jsown:to-json `(:obj
                                  ("modulePaths" . ,(get-module-paths root-path))))))
             ((equal command "inga.getConfig")
              (print-response-msg
                (jsown:val msg "id")
                (jsown:to-json (config-to-obj config))))
             ((equal command "inga.updateConfig")
              (when (first arguments)
                (setf config (obj-to-config (first arguments)))
                (setf inga/analyzer/base::*config* config)
                (with-open-file (out (merge-pathnames ".inga.yml" output-path)
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
                  (format out "~a" (to-yaml config))))
              (print-response-msg
                (jsown:val msg "id")
                (jsown:to-json (config-to-obj config)))))))
        (t
         (setf *processing-msg*
               (process-msg-if-present msg ctx root-path output-path temp-path base-commit
                                       (cdr (assoc :root-host-paths client-params))
                                       config))))))
  (handle-msg params ctx client-params))

(defun process-msg-if-present (msg ctx root-path output-path temp-path base-commit root-host-paths config)
  (when (or (not msg)
            (and *processing-msg* (sb-thread:thread-alive-p *processing-msg*)))
    (return-from process-msg-if-present *processing-msg*))

  (sb-thread:make-thread
    (lambda ()
      (let ((method (jsown:val msg "method")))
        (cond
          ((equal method "textDocument/didChange")
           (log-debug "run textDocument/didChange processing")
           (let* ((path (get-relative-path
                          (subseq (jsown:val (jsown:val (jsown:val msg "params") "textDocument") "uri") 7)
                          root-host-paths))
                  (range (jsown:val (first
                                      (jsown:val (jsown:val msg "params") "contentChanges")) "range"))
                  (start (let ((start (jsown:val range "start")))
                           `((:line . ,(jsown:val start "line"))
                             (:offset . ,(jsown:val start "character")))))
                  (end (let ((start (jsown:val range "end")))
                         `((:line . ,(jsown:val start "line"))
                           (:offset . ,(jsown:val start "character"))))))
             (let ((change-pos (first (find-definitions
                                        `((:path . ,path)
                                          (:start-offset . ,(convert-to-top-offset
                                                              (merge-pathnames path root-path)
                                                              start))
                                          (:end-offset . ,(convert-to-top-offset
                                                            (merge-pathnames path root-path)
                                                            end)))))))
               (when change-pos
                 (with-open-file (out (merge-pathnames "state.json" output-path)
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                   (format out "~a" (to-state-json change-pos root-path)))))))
          ((equal method "inga/diffChanged")
           (log-debug "run inga/diffChanged processing")
           (let* ((path (when (jsown:keyp (jsown:val msg "params") "uri")
                          (get-relative-path
                            (subseq (jsown:val (jsown:val msg "params") "uri") 7) root-host-paths)))
                  (diff (diff-to-ranges (jsown:val (jsown:val msg "params") "diff") root-path)))
             (when (is-analysis-target (context-kind ctx)
                                       path
                                       (context-include ctx)
                                       (context-exclude ctx))
               (update-index (context-ast-index ctx) path))
             (output-report
               (or (analyze
                     ctx diff
                     :success (lambda (results)
                                (output-report results output-path root-path))
                     :failure (lambda (failures)
                                (output-error failures output-path root-path))
                     :config config)
                   `(()))
               output-path root-path)))))
      (process-msg-if-present (dequeue-msg) ctx root-path output-path temp-path base-commit root-host-paths config))))

(defun extract-json (stream)
  ;; Content-Length: 99
  (let* ((input (read-line stream nil))
         (len (when (>= (length input) 16) (parse-integer (subseq input 16)))))
    (unless len (return-from extract-json))
    ;; newline
    (read-line stream)
    ;; JSON
    (let ((buff (make-array len :element-type '(unsigned-byte 8))))
      (read-sequence buff stream)
      (flexi-streams:octets-to-string buff :external-format :utf-8))))

(defmethod log-debug-generic ((mode (eql :server)) content)
  (print-notification-msg
    "window/logMessage"
    (jsown:to-json
      `((:obj
          ("type" . 4)
          ("message" . ,(format nil "~a ~a" (local-time:now) content)))))))

(defmethod log-info-generic ((mode (eql :server)) content)
  (print-notification-msg
    "window/logMessage"
    (jsown:to-json
      `((:obj
          ("type" . 3)
          ("message" . ,(format nil "~a ~a" (local-time:now) content)))))))

(defmethod log-error-generic ((mode (eql :server)) content)
  (print-notification-msg
    "window/logMessage"
    (jsown:to-json
      `((:obj
          ("type" . 1)
          ("message" . ,(format nil "~a ~a" (local-time:now) content)))))))

(defun print-response-msg (id result)
  (unless id
    (return-from print-response-msg))

  (let ((content (format nil "{\"jsonrpc\":\"2.0\",\"id\":\"~a\",\"result\":~a}" id result)))
    (format t (format nil "Content-Length: ~a~c~c~c~c~a" (length content) #\return #\linefeed
                      #\return #\linefeed
                      content))))

(defun print-notification-msg (method params)
  (let ((content (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"~a\",\"params\":~a}" method params)))
    (format t (format nil "Content-Length: ~a~c~c~c~c~a" (length content) #\return #\linefeed
                      #\return #\linefeed
                      content))))

(defun to-state-json (change-pos root-path)
  (jsown:to-json
    `(:obj
       ("didChange" . ,(cons :obj (convert-to-report-pos change-pos root-path))))))

(defun enqueue-msg (msg)
  (unless msg (return-from enqueue-msg))

  (when (equal (jsown:val msg "method") "initialize")
    (setf *msg-q* (make-queue)))
  (unless *msg-q*
    (return-from enqueue-msg))

  (when (equal (jsown:val msg "method") "shutdown")
    (setf *msg-q* (make-queue)))

  (let ((prev (peek-last *msg-q*)))
    (when (and
            prev
            (equal (jsown:val msg "method") "textDocument/didChange")
            (equal (jsown:val prev "method") "textDocument/didChange"))
      (dequeue-last *msg-q*)))
  (enqueue *msg-q* msg))

(defun dequeue-msg ()
  (dequeue *msg-q*))

(defun get-relative-path (path root-host-paths)
  (loop for root-path in root-host-paths
        do
        (when (uiop:string-prefix-p root-path path)
          (return (enough-namestring path root-path)))))

