(defpackage #:inga/language-client/base
  (:use #:cl)
  (:import-from #:jsown)
  (:export #:language-client
           #:make-client
           #:client-process
           #:client-req-id
           #:client-path
           #:start-client
           #:stop-client
           #:initialize-client
           #:references-client
           #:get-command
           #:increment-req-id
           #:exec-command
           #:extract-json))
(in-package #:inga/language-client/base)

(defclass language-client ()
  ((process :initform nil
            :accessor client-process)
   (req-id :initform 0
           :accessor client-req-id)
   (path :initarg :path
         :accessor client-path)
   (id-key :initarg :id-key
           :initform "id"
           :reader client-id-key)))

(defgeneric make-client (kind path)
  (:method (kind path)
    (error 'unknown-client :name kind)))

(defgeneric start-client (client))

(defgeneric stop-client (client))

(defgeneric initialzie-client (client))

(defgeneric references-client (client pos))

(defgeneric get-command (client command))

(defun increment-req-id (client)
  (setf (client-req-id client) (+ (client-req-id client) 1)))

(defun exec-command (client command)
  (format t " command: ~a~%" command)
  (write-line (get-command client command)
              (uiop:process-info-input (client-process client)))
  (force-output (uiop:process-info-input (client-process client)))

  (let ((stream (uiop:process-info-output (client-process client))))
    (loop while stream do
          (let ((result (jsown:parse (extract-json stream))))
            (when (and
                    (jsown:keyp result (client-id-key client))
                    (= (jsown:val result (client-id-key client)) (client-req-id client)))
              (return result)))))
  )

(defun extract-json (stream)
  ;; Content-Length: 99
  (defparameter line (read-line stream))
  (format t " line: ~a~%" line)
  (let ((len (parse-integer (subseq line 16))))
    (format t " len: ~a~%" len)
    ;; newline
    (read-line stream)
    ;; JSON
    (let ((json (read-line stream)))
      (format t " json: ~a, len: ~a~%" json (length json))
      json
      )
    ))
    ;;(format t " before make-array~%")
    ;;(let ((buff (make-array len :initial-element nil)))
    ;;  (format t " after make-array~%")
    ;;  (read-sequence buff stream)
    ;;  (format t " read-sequence~%")
    ;;  ;;(format t " buff: ~a~%" buff)
    ;;  buff
    ;;  )
    ;;))
    ;;(loop
    ;;  with result = ""
    ;;  repeat len
    ;;  do (let ((rchar (read-char stream)))
    ;;       (format t " rchar: ~a~%" rchar)
    ;;       (setf result (format nil "~a~a" result rchar)))
    ;;  finally (progn
    ;;            (format t " extract-json result: ~a~%" result)
    ;;            (return result)))))

