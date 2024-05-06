(defpackage #:inga/language-server
  (:use #:cl)
  (:import-from #:flexi-streams)
  (:import-from #:jsown)
  (:import-from #:inga/ast-index
                #:update-index)
  (:import-from #:inga/git
                #:get-diff)
  (:import-from #:inga/main
                #:analyze
                #:context
                #:context-ast-index
                #:to-json)
  (:export #:run))
(in-package #:inga/language-server)

(defun run (params)
  (destructuring-bind (&key root-path temp-path include exclude base-commit mode) params
    (let* ((ctx (inga/main::start root-path '(:java)
                                  :include include
                                  :exclude exclude
                                  :temp-path temp-path)))
      (handle-msg params ctx)
      (inga/main::stop ctx))))

(defun handle-msg (params ctx)
  (destructuring-bind (&key root-path temp-path include exclude base-commit mode) params
    (let* ((msg (loop while *standard-input* do
                      (let* ((json (extract-json *standard-input*))
                             (result (when json (jsown:parse json))))
                        (return result))))
           (id (when (jsown:keyp msg "id") (jsown:val msg "id")))
           (method (when msg (jsown:val msg "method"))))
      (when method
        (cond
          ((equal method "initialize")
           (print-response-msg id "{\"capabilities\":{}}"))
          ((equal method "shutdown")
           (print-response-msg id "null")
           (return-from handle-msg))
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
           (update-index (context-ast-index ctx)
                         (enough-namestring
                           ;; remove file URI scheme
                           (subseq
                             (jsown:val (jsown:val (jsown:val msg "params") "textDocument") "uri") 7)
                           root-path))
           (let* ((diffs (get-diff root-path base-commit))
                  (results (inga/main:to-json (inga/main:analyze ctx diffs) root-path)))
             (with-open-file (out (merge-pathnames "report/report.json" temp-path)
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
               (format out "~a" results))))))
      (handle-msg params ctx))))

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

(defun print-response-msg (id result)
  (unless id
    (return-from print-response-msg))

  (let ((content (format nil "{\"jsonrpc\":\"2.0\",\"id\":\"~a\",\"result\":~a}" id result)))
    (format t "Content-Length: ~a~c~c~c~c~a" (length content) #\return #\linefeed
            #\return #\linefeed
            content)
    (force-output)))

