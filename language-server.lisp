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
  (destructuring-bind (&key root-path include exclude base-commit mode) params
    (let* ((ctx (inga/main::start root-path '(:java) :include include :exclude exclude)))
      (handle-msg params ctx)
      (inga/main::stop ctx))))

(defun handle-msg (params ctx)
  (destructuring-bind (&key root-path include exclude base-commit mode) params
    (let* ((msg (loop while *standard-input* do
                      (let* ((json (extract-json *standard-input*))
                             (result (when json (jsown:parse json))))
                        (return result))))
           (method (when msg (jsown:val msg "method"))))
      (when method
        (cond
          ((equal method "initialize")
           (format t "Content-Length: 53~c~c~c~c~a" #\return #\linefeed
                   #\return #\linefeed
                   "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"capabilities\":{}}}")
           (force-output))
          ((or (equal method "initialized")
               (equal method "textDocument/didChange"))
           (let* ((diffs (get-diff root-path base-commit))
                  (results (inga/main:to-json (inga/main:analyze ctx diffs) root-path)))
             (ensure-directories-exist "reports/")
             (with-open-file (out "reports/report.json"
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
               (format out "~a" results))
             (format t "~%~a~%" results)))))
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

