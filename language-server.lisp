(defpackage #:inga/language-server
  (:use #:cl)
  (:import-from #:flexi-streams)
  (:import-from #:jsown)
  (:import-from #:inga/git
                #:get-diff)
  (:import-from #:inga/main
                #:analyze
                #:to-json)
  (:export #:run))
(in-package #:inga/language-server)

(defun run (params)
  (let* ((msg (loop while *standard-input* do
                    (let* ((json (extract-json *standard-input*))
                           (result (when json (jsown:parse json))))
                      (return result))))
         (method (when msg (jsown:val msg "method"))))
    (when method
      (cond
        ((equal method "initialize")
         (format t "Content-Length: 53~C~%~C~%~a~%" #\return #\return
                 "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"capabilities\":{}}}")
         (force-output t))
        ((equal method "initialized")
         (destructuring-bind (&key root-path include exclude base-commit mode) params
           (let* ((diffs (get-diff root-path base-commit))
                  (ctx (inga/main::start root-path
                              '(:java)
                              :include include :exclude exclude))
                  (results (inga/main:to-json (inga/main:analyze ctx diffs) root-path)))
             ;; TODO: count length
             (format t "Content-Length: 100~C~%~C~%~%" #\return #\return)
             (format t "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":~a}" results)
             (force-output t))))
        ((equal method "textDocument/didChange")
         )))
    (run params)))

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

