(defpackage #:inga/server
  (:use #:cl
        #:inga/utils)
  (:import-from #:flexi-streams)
  (:import-from #:jsown)
  (:import-from #:local-time)
  (:import-from #:inga/analyzer
                #:analyze
                #:find-definitions 
                #:start-analyzer
                #:stop-analyzer)
  (:import-from #:inga/ast-index
                #:ast-index-disk
                #:update-index)
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defparameter *msg-q* nil)
(defparameter *processing-msg* nil)
(defparameter *output-q* (make-queue))
(defparameter *processing-output* nil)
(defparameter *stdout-q* (sb-concurrency:make-queue))
(defparameter *stdout-thread* nil)

(defmethod run ((mode (eql :server)) language params)
  (let* ((index (make-instance 'ast-index-disk
                               :root-path (cdr (assoc :root-path params))
                               :temp-path (cdr (assoc :temp-path params))))
         (ctx (case language
                (t
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
                                   (cdr (assoc :root-path params)))))))))
    (init-msg-q)
    (handle-msg params ctx)))

(defun handle-msg (params ctx &optional root-host-paths)
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
         (when (jsown:val (jsown:val msg "params") "rootUri")
           (push (namestring (pathname
                               (concatenate
                                 'string
                                 ;; remove file URI scheme (file://)
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
         (print-response-msg (jsown:val msg "id") "{\"capabilities\":{\"textDocumentSync\":{\"change\":2,\"save\":false}}}")
         (process-output-if-present `(()) output-path root-path))
        ((equal (jsown:val msg "method") "shutdown")
         (log-debug "run shutdown processing")

         (loop for p in (context-processes ctx) do (uiop:close-streams p)) 
         (loop for a in (context-analyzers ctx) do (stop-analyzer a))

         (print-response-msg (jsown:val msg "id") "null")
         (return-from handle-msg))
        (t
         (setf *processing-msg*
               (process-msg-if-present msg ctx root-path output-path temp-path base-commit root-host-paths config))))))
  (handle-msg params ctx root-host-paths))

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
             (process-output-if-present
               (or (analyze
                     ctx diff
                     :success (lambda (results)
                                (process-output-if-present results output-path root-path))
                     :failure (lambda (failures)
                                (output-error failures output-path root-path))
                     :config config)
                   `(()))
               output-path root-path)))))
      (process-msg-if-present (dequeue-msg) ctx root-path output-path temp-path base-commit root-host-paths config))))

(defun process-output-if-present (output output-path root-path)
  (unless output
    (return-from process-output-if-present))

  (enqueue-output output)
  (unless *processing-output*
    (setf *processing-output*
          (sb-thread:make-thread
            (lambda ()
              (let ((report (dequeue-output)))
                (output-report report output-path root-path)
                (setf *processing-output* nil)
                (process-output-if-present (dequeue-output) output-path root-path)))))))

(defun extract-json (stream)
  ;; Content-Length: 99
  (let* ((input (read-line stream nil))
         (len (when (>= (length input) 16) (parse-integer (subseq input 16)))))
    (unless len (return-from extract-json))
    ;; newline
    (read-line stream)
    ;; JSON
    (loop
      with buff = (make-array len :fill-pointer 0)
      repeat len
      do (vector-push (read-byte stream) buff)
      finally (return (flexi-streams:octets-to-string buff :external-format :utf-8)))))

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
    (print-if-present (format nil "Content-Length: ~a~c~c~c~c~a" (length content) #\return #\linefeed
                              #\return #\linefeed
                              content))))

(defun print-notification-msg (method params)
  (let ((content (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"~a\",\"params\":~a}" method params)))
    (print-if-present (format nil "Content-Length: ~a~c~c~c~c~a" (length content) #\return #\linefeed
                              #\return #\linefeed
                              content))))

(defun print-if-present (json)
  (unless json
    (return-from print-if-present))
  
  (sb-concurrency:enqueue json *stdout-q*) 
  (when (or (null *stdout-thread*) (not (sb-thread:thread-alive-p *stdout-thread*)))
    (setf *stdout-thread*
          (sb-thread:make-thread
            (lambda ()
              (format t (sb-concurrency:dequeue *stdout-q*))
              (force-output)
              (print-if-present (sb-concurrency:dequeue *stdout-q*)))))))

(defun to-state-json (change-pos root-path)
  (jsown:to-json
    `(:obj
       ("didChange" . ,(cons :obj (convert-to-report-pos change-pos root-path))))))

(defun init-msg-q ()
  (setf *msg-q* (make-queue)))

(defun enqueue-msg (msg)
  (unless msg (return-from enqueue-msg))

  (when (equal (jsown:val msg "method") "shutdown")
    (init-msg-q))

  (let ((prev (peek-last *msg-q*)))
    (when (and
            prev
            (equal (jsown:val msg "method") "textDocument/didChange")
            (equal (jsown:val prev "method") "textDocument/didChange"))
      (dequeue-last *msg-q*)))
  (enqueue *msg-q* msg))

(defun dequeue-msg ()
  (dequeue *msg-q*))

(defun enqueue-output (output)
  (unless output (return-from enqueue-output))

  (let ((prev (peek-last *output-q*)))
    (when prev
      (dequeue-last *output-q*)))
  (enqueue *output-q* output))

(defun dequeue-output ()
  (dequeue *output-q*))

(defun get-relative-path (path root-host-paths)
  (loop for root-path in root-host-paths
        do
        (when (uiop:string-prefix-p root-path path)
          (return (enough-namestring path root-path)))))

