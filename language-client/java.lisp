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

(defmethod start-client ((client language-client-java))
  (let ((home (uiop:getenv "INGA_HOME")))
    (setf (client-process client)
          (uiop:launch-program
            (format nil "~a/libs/jdtls/bin/jdtls -data ~a/libs/jdtls/workspace --jvm-arg=-javaagent:~a/libs/lombok.jar" home home home)
            :input :stream :output :stream)))
  (initialize-client client))

(defmethod stop-client ((client language-client-java))
  (uiop:close-streams (client-process client)))

(defmethod references-client ((client language-client-java) pos)
  (let ((cache (get-value (client-cache client) (get-references-key pos)))
        (full-path (namestring
                     (uiop:merge-pathnames* (cdr (assoc :path pos))
                                            (client-path client)))))
    (values
      (if cache
          cache
          (progn
            (increment-req-id client)
            (let ((refs (exec-command client (format nil "{\"jsonrpc\":\"2.0\",\"id\":~a,\"method\":\"textDocument/references\",\"params\":{\"textDocument\":{\"uri\":\"file://~a\"},\"position\":{\"line\":~a,\"character\":~a},\"context\":{\"includeDeclaration\":false}}}"
                                                     (client-req-id client) full-path
                                                     ;; the zero-based line and offset
                                                     (- (cdr (assoc :line pos)) 1) (- (cdr (assoc :offset pos)) 1))))
                  result)
              (setf result (mapcar
                             (lambda (ref)
                               (list
                                 (cons :path (enough-namestring (subseq (jsown:val ref "uri") 7)
                                                                (client-path client)))
                                 (cons :line (+ (jsown:val (jsown:val (jsown:val ref "range") "start") "line") 1))
                                 (cons :offset (+ (jsown:val (jsown:val (jsown:val ref "range") "start") "character") 1))))
                             (jsown:val refs "result")))
              (put-value (client-cache client) (get-references-key pos) result)
              result)))
      (when cache t))))

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

