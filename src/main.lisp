(defpackage inga
  (:use :cl)
  (:export #:analyze))
(in-package :inga)

;; TODO: フルパスを止めたい
(defparameter *tsserver* (uiop:launch-program "/Users/seito/.nvm/versions/node/v14.16.0/bin/tsserver" :input :stream :output :stream))

(defun analyze ()
  ;;(uiop:process-alive-p *tsserver*)

  (write-line "{\"seq\": 1, \"command\": \"open\", \"arguments\": {\"file\": \"/Users/seito/git/GitHub/vscode-pomodoro-edit/src/extension.ts\"}}" (uiop:process-info-input *tsserver*))
  (force-output (uiop:process-info-input *tsserver*))
  (let ((stream (uiop:process-info-output *tsserver*)))
       (loop while (listen stream) do
           ;; Characters are immediately available
           (princ (read-line stream))
           (terpri)))

  (uiop:close-streams *tsserver*)

  (+ 1 1))
