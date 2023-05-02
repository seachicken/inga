(defpackage #:inga/language-client/java
  (:use #:cl
        #:inga/language-client/base)
  (:import-from #:inga/cache
                #:put-value
                #:get-value)
  (:export #:language-client-java))
(in-package #:inga/language-client/java)

(defclass language-client-java (language-client)
  ())

(defmethod make-client ((kind (eql :java)) path cache)
  (make-instance 'language-client-java :path path :cache cache))

(defmethod start-client ((client language-client-java)))

(defmethod stop-client ((client language-client-java)))

(defmethod references-client ((client language-client-java) pos))

(defmethod get-command ((client language-client-java) command)
  (format nil "Content-Length: ~a~c~c~c~c~a"
          (length command) #\return #\linefeed
          #\return #\linefeed
          command))

(defun initialize-client (client)
  (increment-req-id client)
  (exec-command client (format nil "{\"jsonrpc\":\"2.0\",\"id\":~a,\"method\":\"initialize\",\"params\":{\"processId\":~a,\"rootUri\":\"file://~a\",\"capabilities\":{}}}"
                (client-req-id client) (sb-posix:getpid) (client-path client)))
  (let ((stream (uiop:process-info-output (client-process client))))
    (loop while stream do
          (let ((result (jsown:parse (extract-json stream))))
            (when (and
                    (jsown:keyp result "params")
                    (jsown:keyp (jsown:val result "params") "type")
                    (equal (jsown:val (jsown:val result "params") "type") "Started"))
              (return))))))

