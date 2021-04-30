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
  (find-item body *diff-line*)
  (format t "found deepest-item=~a~%" *deepest-item*)

  '(:pos ("line" . 11) ("offset" . 12)))

(defvar *deepest-item*)

(defun find-item (tree line)
  (when (contains-line tree line)
      (format t "found!! tree=~a%" tree)
      (setq *deepest-item* tree))

  (if (jsown:keyp tree "childItems")
      (progn
        (format t "has child~%")
        (find-item (jsown:val tree "childItems") line))
      (when (jsown:keyp tree "OBJ")
        (format t "has OBJ list~%")
        (loop for obj in tree do
              (format t "objk=~a, text=~a, kind=~a~%"
                      (jsown:keywords obj)
                      (jsown:val obj "text")
                      (jsown:val obj "kind"))
              (find-item obj line)))))

(defun contains-line (tree line)
  ;;(format t "key=~a~%" (jsown:keywords tree))
  (when (and (jsown:keyp tree "text") (not (string= (jsown:val tree "kind") "module")))
    ;;(format t "text=~a, spans=~a~%" (jsown:val tree "text") (jsown:val tree "spans"))
    (let ((span (car (jsown:val tree "spans"))) start end)
      (setq start (jsown:val (jsown:val span "start") "line"))
      (setq end (jsown:val (jsown:val span "end") "line"))
      (and (>= line start) (>= end line))))
  )

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

