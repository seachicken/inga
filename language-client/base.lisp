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

(defgeneric references-client (client file-path pos))

(defgeneric get-command (client command))

(defun increment-req-id (client)
  (setf (client-req-id client) (+ (client-req-id client) 1)))

(defun exec-command (client command)
  (write-line (get-command client command)
              (uiop:process-info-input (client-process client)))
  (force-output (uiop:process-info-input (client-process client)))

  (let ((stream (uiop:process-info-output (client-process client))))
    (loop while stream do
          (let ((result (jsown:parse (extract-json stream))))
            (when (and
                    (jsown:keyp result (client-id-key client))
                    (= (jsown:val result (client-id-key client)) (client-req-id client)))
              (return result))))))

(defun extract-json (stream)
  ;; Content-Length: 99
  (let ((len (parse-integer (subseq (read-line stream) 16))))
    ;; newline
    (read-line stream)
    ;; JSON
    (loop
      with result = ""
      repeat len
      do (setf result (format nil "~a~a" result (read-char stream)))
      finally (return result))))

