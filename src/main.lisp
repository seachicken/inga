(defpackage inga
  (:use :cl :jsown)
  (:export #:start #:stop #:analyze))
(in-package :inga)

(defvar *tsserver*)

(defun start ()
  (print "hello")
  ;; TODO: フルパスを止めたい
  (setq *tsserver* (uiop:launch-program "/Users/seito/.nvm/versions/node/v14.16.0/bin/tsserver" :input :stream :output :stream)))

(defun stop ()
  (print "bye")
  (uiop:close-streams *tsserver*))

(defun analyze ()
  ;; TODO: diffから動的に取得する
  (defparameter diff-line 13)

  (print "open")
  (call "{\"seq\": 1, \"command\": \"open\", \"arguments\": {\"file\": \"/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/tests/fixtures/create-react-app-typescript-todo-example-2020/src/App/NewTodoInput/index.tsx\"}}")

  (print "navtree")
  (defvar result)
  (setq result (exec "{\"seq\": 2, \"command\": \"navtree\", \"arguments\": {\"file\": \"/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/tests/fixtures/create-react-app-typescript-todo-example-2020/src/App/NewTodoInput/index.tsx\"}}"))
  (format t "result=~a~%" (jsown:val result "type"))

  '(:pos ("line" . 11) ("offset" . 12)))

(defun exec (command)
  (call command)
  (let ((stream (uiop:process-info-output *tsserver*)))
       (loop while stream do
             (defvar result)
             (setq result (jsown:parse (extjson stream)))
             (format t "r=~a~%" result)
             (when (string= (jsown:val result "type") "response")
               (return result)))))

(defun call (command)
  (write-line command (uiop:process-info-input *tsserver*))
  (force-output (uiop:process-info-input *tsserver*)))

(defun extjson (stream)
  ;; Content-Length: 99
  (read-line stream)
  ;; 改行コード
  (read-line stream)
  ;; JSON 
  (read-line stream))

