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
                #:context-lc
                #:context-processes)
  (:import-from #:inga/file
                #:convert-to-pos 
                #:convert-to-top-offset)
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
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path))
(in-package #:inga/server)

(defparameter *msg-q* nil)
(defparameter *processing-msg* nil)

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
         (print-response-msg (jsown:val msg "id") "{\"capabilities\":{\"textDocumentSync\":{\"change\":2,\"save\":false}}}"))
        ((equal (jsown:val msg "method") "shutdown")
         (log-debug "run shutdown processing")

         (loop for p in (context-processes ctx) do (uiop:close-streams p)) 
         (loop for a in (context-analyzers ctx) do (stop-analyzer a))

         (print-response-msg (jsown:val msg "id") "null")
         (return-from handle-msg))
        (t
         (setf *processing-msg*
               (process-msg-if-present msg ctx root-path output-path temp-path base-commit root-host-paths))))))
  (handle-msg params ctx root-host-paths))

(defun process-msg-if-present (msg ctx root-path output-path temp-path base-commit root-host-paths)
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
                  (diff (diff-to-ranges (jsown:val (jsown:val msg "params") "diff") root-path))
                  (results (to-json (analyze ctx diff) root-path)))
             (when path (update-index (context-ast-index ctx) path))
             (with-open-file (out (merge-pathnames "report.json" output-path)
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
               (format out "~a" results))))))
      (process-msg-if-present (dequeue-msg) ctx root-path output-path temp-path base-commit root-host-paths))))

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
    (format t "Content-Length: ~a~c~c~c~c~a" (length content) #\return #\linefeed
            #\return #\linefeed
            content)
    (force-output)))

(defun print-notification-msg (method params)
  (let ((content (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"~a\",\"params\":~a}" method params)))
    (format t "Content-Length: ~a~c~c~c~c~a" (length content) #\return #\linefeed
            #\return #\linefeed
            content)
    (force-output)))

(defun to-state-json (change-pos root-path)
  (jsown:to-json
    `(:obj
       ("didChange" . ,(cons :obj (key-downcase (convert-to-output-pos root-path change-pos)))))))

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

(defun get-relative-path (path root-host-paths)
  (loop for root-path in root-host-paths
        do
        (when (uiop:string-prefix-p root-path path)
          (return (enough-namestring path root-path)))))

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
                                                          (cdr (assoc :entrypoint r))))))))))
                (when (equal (cdr (assoc :type r)) "entrypoint")
                  (push (cons "service"
                              (first (last (pathname-directory
                                             (find-base-path
                                               (merge-pathnames
                                                 (cdr (assoc :path
                                                             (convert-to-output-pos
                                                               root-path
                                                               (cdr (assoc :entrypoint r)))))
                                                 root-path))))))
                        (cdr (assoc :obj obj))))
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

