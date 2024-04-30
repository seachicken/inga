(defpackage #:inga/language-server
  (:use #:cl)
  (:import-from #:flexi-streams)
  (:import-from #:jsown)
  (:export #:run))
(in-package #:inga/language-server)

(defun run ()
  (let ((msg (loop while *standard-input* do
                   (let* ((json (extract-json *standard-input*))
                          (result (when json (jsown:parse json))))
                     (return result)))))
    (when msg
      (format t "Content-Length: 53~C~%~C~%~a~%" #\return #\return
              "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"capabilities\":{}}}")
      (force-output t))
    (run)))

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

