(defpackage #:inga/language-client/typescript
  (:use #:cl
        #:inga/language-client/base)
  (:import-from #:inga/cache
                #:put-value
                #:get-value)
  (:import-from #:inga/ast-analyzer
                #:convert-to-pos
                #:convert-to-top-offset)
  (:export #:language-client-typescript))
(in-package #:inga/language-client/typescript)

(defclass language-client-typescript (language-client)
  ())

(defmethod make-client ((kind (eql :typescript)) path cache)
  (make-instance 'language-client-typescript
                 :path path :id-key "request_seq" :cache cache))

(defmethod start-client ((client language-client-typescript))
  (setf (client-process client)
        (uiop:launch-program
          "tsserver"
          :input :stream :output :stream)))

(defmethod stop-client ((client language-client-typescript))
  (uiop:close-streams (client-process client)))

(defmethod references-client ((client language-client-typescript) pos)
  (let ((cache (get-value (client-cache client) (get-references-key pos)))
        (text-pos (convert-to-pos (client-path client) (cdr (assoc :path pos))
                                  (cdr (assoc :top-offset pos))))
        (full-path (namestring
                     (uiop:merge-pathnames* (cdr (assoc :path pos))
                                            (client-path client)))))
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
                                                     (cdr (assoc :line text-pos)) (cdr (assoc :offset text-pos)))))
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
                                  (unless (= (cdr (assoc :line ref-pos)) (cdr (assoc :line text-pos)))
                                    (let ((top-offset
                                            (convert-to-top-offset (client-path client)
                                                                   (cdr (assoc :path ref-pos))
                                                                   ref-pos)))
                                      (list
                                        (cons :path (cdr (assoc :path ref-pos)))
                                        (cons :top-offset top-offset))))))
                              (jsown:val (jsown:val refs "body") "refs"))))
              (put-value (client-cache client) (get-references-key pos) result)
              result)))
      (when cache t))))

(defmethod get-command ((client language-client-typescript) command)
  command)

(defun exec (client command)
  (write-line command
              (uiop:process-info-input (client-process client)))
  (force-output (uiop:process-info-input (client-process client))))

