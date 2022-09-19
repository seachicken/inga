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

(defmethod references-client ((client language-client-typescript) pos)
  (increment-req-id client)
  (exec client (format nil "{\"seq\": ~a, \"command\": \"open\", \"arguments\": {\"file\": \"~a\"}}"
                       (client-req-id client) (cdr (assoc :path pos))))

  (increment-req-id client)
  (let ((refs (exec-command client (format nil "{\"seq\": ~a, \"command\": \"references\", \"arguments\": {\"file\": \"~a\", \"line\": ~a, \"offset\": ~a}}"
                              (client-req-id client) (cdr (assoc :path pos))
                              (cdr (assoc :line pos)) (cdr (assoc :offset pos))))))
    (mapcar (lambda (ref)
              (list
                (cons :path (jsown:val ref "file"))
                (cons :line (jsown:val (jsown:val ref "start") "line"))
                (cons :offset (jsown:val (jsown:val ref "start") "offset"))))
            (jsown:val (jsown:val refs "body") "refs"))))

(defmethod get-command ((client language-client-typescript) command)
  command)

(defun exec (client command)
  (write-line command
              (uiop:process-info-input (client-process client)))
  (force-output (uiop:process-info-input (client-process client))))

