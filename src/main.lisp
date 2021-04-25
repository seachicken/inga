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
  ;;(uiop:process-alive-p *tsserver*)

  (print "open")
  (call "{\"seq\": 1, \"command\": \"open\", \"arguments\": {\"file\": \"/Users/seito/git/GitHub/vscode-pomodoro-edit/src/extension.ts\"}}")

  (print "navtree")
  (defvar result)
  (setq result (exec "{\"seq\": 2, \"command\": \"navtree\", \"arguments\": {\"file\": \"/Users/seito/git/GitHub/vscode-pomodoro-edit/src/extension.ts\"}}"))
  (format t "result=~a~%" (jsown:val result "type"))

  (+ 1 1))

(defun call (command)
  (write-line command (uiop:process-info-input *tsserver*))
  (force-output (uiop:process-info-input *tsserver*)))

(defun exec (command)
  (call command)
  (let ((stream (uiop:process-info-output *tsserver*)))
       (loop while stream do
             (defvar result)
             (setq result (jsown:parse (extjson stream)))
             (when (string= (jsown:val result "type") "response")
               (return result)))))

(defun extjson (stream)
  ;; Content-Length: 99
  (read-line stream)
  ;; 改行コード
  (read-line stream)
  ;; JSON 
  (read-line stream))

