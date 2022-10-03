(defpackage #:inga/language-client/typescript
  (:use #:cl
        #:inga/language-client/base)
  (:export #:language-client-typescript))
(in-package #:inga/language-client/typescript)

(defclass language-client-typescript (language-client)
  ())

(defmethod make-client ((kind (eql :typescript)) path)
  (make-instance 'language-client-typescript
                 :path path :id-key "request_seq"))

(defmethod start-client ((client language-client-typescript))
  (setf (client-process client)
        (uiop:launch-program
          "tsserver"
          :input :stream :output :stream)))

(defmethod stop-client ((client language-client-typescript))
  (uiop:close-streams (client-process client)))

(defmethod initialize-client ((client language-client-typescript))
  (increment-req-id client)
  (exec client (format nil "{\"seq\": ~a, \"command\": \"open\", \"arguments\": {\"file\": \"~a\"}}"
                               (client-req-id client) (client-path client)))
  (let ((stream (uiop:process-info-output (client-process client))))
    (loop while stream do
          (defvar json (extract-json stream))
          (format t " json: ~a~%" json)
          (let ((result (jsown:parse json)))
            (format t " initialize result: ~a~%" result)
            (when (and
                    (jsown:keyp result "params")
                    (jsown:keyp (jsown:val result "params") "type")
                    (equal (jsown:val (jsown:val result "params") "type") "Started"))
              (return))))))

(defmethod references-client ((client language-client-typescript) pos)
  (let ((full-path (namestring
                     (uiop:merge-pathnames* (cdr (assoc :path pos))
                                            (client-path client)))))
    (format t " full-path: ~a~%" full-path)
    (increment-req-id client)
    (exec client (format nil "{\"seq\": ~a, \"command\": \"open\", \"arguments\": {\"file\": \"~a\"}}"
                         (client-req-id client) full-path))
    (let ((stream (uiop:process-info-output (client-process client))))
      (loop while (listen stream) do
            (defvar json (extract-json stream))
            (let ((result (jsown:parse json)))
              (format t " open result: ~a~%" result))))
    (format t " end open~%")

    (increment-req-id client)
    (let ((refs (exec-command client (format nil "{\"seq\": ~a, \"command\": \"references\", \"arguments\": {\"file\": \"~a\", \"line\": ~a, \"offset\": ~a}}"
                                (client-req-id client) full-path
                                (cdr (assoc :line pos)) (cdr (assoc :offset pos))))))
      (format t " refs: ~a~%" (inga/utils::top (format nil "~a" refs) 5))
      (mapcar (lambda (ref)
                (list
                  (cons :path (enough-namestring (jsown:val ref "file")
                                                 (client-path client)))
                  (cons :line (jsown:val (jsown:val ref "start") "line"))
                  (cons :offset (jsown:val (jsown:val ref "start") "offset"))))
              (jsown:val (jsown:val refs "body") "refs")))))

(defmethod get-command ((client language-client-typescript) command)
  command)

(defun exec (client command)
  (write-line command
              (uiop:process-info-input (client-process client)))
  (force-output (uiop:process-info-input (client-process client))))

