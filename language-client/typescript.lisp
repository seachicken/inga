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
  (let ((key (list (cdr (assoc :path pos)) (cdr (assoc :line pos)) (cdr (assoc :offset pos))))
        cache
        (full-path (namestring
                     (uiop:merge-pathnames* (cdr (assoc :path pos))
                                            (client-path client)))))
    (setf cache (cdr (assoc key (client-cache-refs client) :test #'equal)))
    (values
      (if cache
          cache
          (progn
            (increment-req-id client)
            (exec client (format nil "{\"seq\": ~a, \"command\": \"open\", \"arguments\": {\"file\": \"~a\"}}"
                                 (client-req-id client) full-path))

            (increment-req-id client)
            (let ((refs (exec-command client (format nil "{\"seq\": ~a, \"command\": \"references\", \"arguments\": {\"file\": \"~a\", \"line\": ~a, \"offset\": ~a}}"
                                                     (client-req-id client) full-path
                                                     (cdr (assoc :line pos)) (cdr (assoc :offset pos)))))
                  result)
              (setf result
                    (remove
                      nil
                      (mapcar (lambda (ref)
                                (let ((ref-pos (list
                                                 (cons :path (enough-namestring (jsown:val ref "file")
                                                                                (client-path client)))
                                                 (cons :line (jsown:val (jsown:val ref "start") "line"))
                                                 (cons :offset (jsown:val (jsown:val ref "start") "offset")))))
                                  (unless (= (cdr (assoc :line ref-pos)) (cdr (assoc :line pos)))
                                    ref-pos)))
                              (jsown:val (jsown:val refs "body") "refs"))))
              (push (cons
                      (list (cdr (assoc :path pos)) (cdr (assoc :line pos)) (cdr (assoc :offset pos)))
                      result)
                    (client-cache-refs client))
              result)))
      (when cache t))))

(defmethod get-command ((client language-client-typescript) command)
  command)

(defun exec (client command)
  (write-line command
              (uiop:process-info-input (client-process client)))
  (force-output (uiop:process-info-input (client-process client))))

