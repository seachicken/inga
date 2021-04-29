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
  (defparameter *diff-line* 13)

  (print "open")
  (call "{\"seq\": 1, \"command\": \"open\", \"arguments\": {\"file\": \"/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/tests/fixtures/create-react-app-typescript-todo-example-2020/src/App/NewTodoInput/index.tsx\"}}")

  (print "navtree")
  (defvar result)
  (setq result (exec "{\"seq\": 2, \"command\": \"navtree\", \"arguments\": {\"file\": \"/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/tests/fixtures/create-react-app-typescript-todo-example-2020/src/App/NewTodoInput/index.tsx\"}}"))
  (format t "resultk=~a~%" (jsown:keywords result))
  (defparameter body (jsown:val result "body"))
  (print (find-item body *diff-line*))
  ;;(format t "body=~a~%" body)
  ;;(format t "bodyk=~a, spans=~a~%" (jsown:keywords body) (jsown:val body "spans"))

  '(:pos ("line" . 11) ("offset" . 12)))

(defun find-item (tree line)
  (if (null tree)
      nil
      (progn
        (format t "treek=~a~%" (jsown:keywords tree))
        (when (jsown:keyp tree "text")
          (format t "text=~a, spans=~a~%" (jsown:val tree "text") (jsown:val tree "spans"))
          (when (contains-line (jsown:val tree "spans") line)
            (print "!!!!!!!!!!!!!!")
            tree))
        (if (jsown:keyp tree "childItems")
            (progn
              (format t "has child~%")
              (find-item (jsown:val tree "childItems") line))
            (progn
              (format t "has not child~%")
              (when (jsown:keyp tree "OBJ")
                (loop for obj in tree do
                      (format t "objk=~a~%" (jsown:keywords obj))
                      (find-item obj line))))))))

(defun contains-line (spans line)
  (format t "spans=~a~%" spans)
  (let ((span (car spans)) start end)
    (setq start (jsown:val (jsown:val span "start") "line"))
    (setq end (jsown:val (jsown:val span "end") "line"))
    (and (>= line start) (>= end line))))

(defun exec (command)
  (call command)
  (let ((stream (uiop:process-info-output *tsserver*)))
       (loop while stream do
             (defvar result)
             (setq result (jsown:parse (extjson stream)))
             ;;(format t "r=~a~%" result)
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

