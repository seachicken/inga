(defpackage #:inga/language-server
  (:use #:cl
        #:inga/utils)
  (:import-from #:flexi-streams)
  (:import-from #:jsown)
  (:import-from #:local-time)
  (:import-from #:inga/ast-index
                #:update-index)
  (:import-from #:inga/git
                #:get-diff)
  (:import-from #:inga/logger
                #:log-error
                #:log-error-generic
                #:log-info-generic)
  (:import-from #:inga/main
                #:analyze
                #:context
                #:context-ast-index
                #:convert-to-output-pos
                #:key-downcase
                #:to-json)
  (:import-from #:inga/traversal
                #:convert-to-top-offset
                #:find-definitions)
  (:export #:run))
(in-package #:inga/language-server)

(defparameter *msg-q* nil)
(defparameter *processing-msg* nil)

(defun run (params)
  (destructuring-bind (&key root-path temp-path include exclude base-commit mode) params
    (let* ((ctx (inga/main::start root-path '(:java)
                                  :include include
                                  :exclude exclude
                                  :temp-path temp-path)))
      (init-msg-q)
      (handle-msg params ctx)
      (inga/main::stop ctx))))

(defun handle-msg (params ctx &optional root-host-paths)
  (destructuring-bind (&key root-path temp-path include exclude base-commit mode) params
    (let ((msg (loop while *standard-input* do
                     (let* ((json (extract-json *standard-input*))
                            (result (when json (jsown:parse json))))
                       (return result)))))
      (when msg (enqueue-msg msg))
      (setf msg (dequeue-msg))
      (when msg
        (cond
          ((equal (jsown:val msg "method") "initialize")
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
           (print-response-msg (jsown:val msg "id") "null")
           (return-from handle-msg))
          (t
           (setf *processing-msg*
                 (process-msg-if-present msg ctx root-path temp-path base-commit root-host-paths))))))
    (handle-msg params ctx root-host-paths)))

(defun process-msg-if-present (msg ctx root-path temp-path base-commit root-host-paths)
  (when (or (not msg)
            (and *processing-msg* (sb-thread:thread-alive-p *processing-msg*)))
    (return-from process-msg-if-present *processing-msg*))

  (sb-thread:make-thread
    (lambda ()
      (let ((method (jsown:val msg "method")))
        (cond
          ((equal method "initialized")
           (let* ((diffs (get-diff root-path base-commit))
                  (results (inga/main:to-json (inga/main:analyze ctx diffs) root-path)))
             (ensure-directories-exist (merge-pathnames "report/" temp-path))
             (with-open-file (out (merge-pathnames "report/report.json" temp-path)
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
               (format out "~a" results))))
          ((equal method "textDocument/didChange")
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
             (if (probe-file (merge-pathnames path root-path))
                 (let ((change-pos (first (find-definitions
                                            `((:path . ,path)
                                              (:start-offset . ,(convert-to-top-offset
                                                                  (merge-pathnames path root-path)
                                                                  start))
                                              (:end-offset . ,(convert-to-top-offset
                                                                (merge-pathnames path root-path)
                                                                end)))))))
                   (with-open-file (out (merge-pathnames "report/state.json" temp-path)
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                     (format out "~a" (to-state-json change-pos root-path))))
                 (log-error (format nil "~a is not found" path)))))
          ((equal method "textDocument/didSave")
           (let ((path (get-relative-path
                         (subseq (jsown:val (jsown:val (jsown:val msg "params") "textDocument") "uri") 7)
                         root-host-paths)))
             (if (probe-file (merge-pathnames path root-path))
                 (update-index (context-ast-index ctx) path)
                 (log-error (format nil "~a is not found" path)))
             (let* ((diffs (get-diff root-path base-commit))
                    (results (inga/main:to-json (inga/main:analyze ctx diffs) root-path)))
               (with-open-file (out (merge-pathnames "report/report.json" temp-path)
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
                 (format out "~a" results)))))))
      (process-msg-if-present (dequeue-msg) ctx root-path temp-path base-commit root-host-paths))))

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

(defmethod log-error-generic ((mode (eql :server)) content)
  (print-notification-msg
    "window/logMessage"
    (format nil "{\"type\":1,\"message\":\"~a ~a\"}" (local-time:now) content)))

(defmethod log-info-generic ((mode (eql :server)) content)
  (print-notification-msg
    "window/logMessage"
    (format nil "{\"type\":3,\"message\":\"~a ~a\"}" (local-time:now) content)))

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
