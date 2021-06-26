(defpackage inga
  (:use :cl :jsown)
  (:export #:start #:stop #:analyze #:find-components))
(in-package :inga)

(defvar *deepest-item*)
(defvar *tsserver*)
(defvar *tsparser*)

(defun start ()
  (print "hello")
  ;; TODO: フルパスを止めたい
  (setq *tsserver* (uiop:launch-program "/Users/seito/.nvm/versions/node/v14.17.0/bin/tsserver" :input :stream :output :stream)))

(defun stop ()
  (print "bye")
  (uiop:close-streams *tsserver*))

(defun start-tsparser ()
  (print "start tsparser")
  ;; TODO: フルパスを止めたい
  (setq *tsparser* (uiop:launch-program "node /Users/seito/git/GitHub/tsparser/bin/tsparser" :input :stream :output :stream)))

(defun stop-tsparser ()
  (print "stop tsparser")
  (uiop:close-streams *tsparser*))

(defun analyze ()
  ;; TODO: diffから動的に取得する
  (defparameter *diff-line* 13)

  (print "open")
  (call "{\"seq\": 1, \"command\": \"open\", \"arguments\": {\"file\": \"/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/tests/fixtures/create-react-app-typescript-todo-example-2020/src/App/NewTodoInput/index.tsx\"}}")

  (print "navtree")
  (defvar result)
  (setq result (exec "{\"seq\": 2, \"command\": \"navtree\", \"arguments\": {\"file\": \"/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/tests/fixtures/create-react-app-typescript-todo-example-2020/src/App/NewTodoInput/index.tsx\"}}"))
  (defparameter body (jsown:val result "body"))
  (find-item body *diff-line*)
  (format t "found deepest-item=~a~%" *deepest-item*)
  (get-pos *deepest-item*))

(defun find-components (pos)
  (print "open")
  (call "{\"seq\": 1, \"command\": \"open\", \"arguments\": {\"file\": \"/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/tests/fixtures/create-react-app-typescript-todo-example-2020/src/App/NewTodoInput/index.tsx\"}}")

  (print "references")
  (defparameter result
    (exec (format nil "{\"seq\": 2, \"command\": \"references\", \"arguments\": {\"file\": \"/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/tests/fixtures/create-react-app-typescript-todo-example-2020/src/App/NewTodoInput/index.tsx\", \"line\": ~a, \"offset\": ~a}}"
                  (jsown:val pos "line") (jsown:val pos "offset"))))
  (defparameter body (jsown:val result "body"))
  (defparameter refs (jsown:val body "refs"))
  (remove-if (lambda (p) (equal p pos))
             (mapcar (lambda (r) (cons :pos (cdr (jsown:val r "start")))) refs))
  (format t "refs=~a~%" refs)
  (start-tsparser)
  (exec-tsparser "/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/tests/fixtures/create-react-app-typescript-todo-example-2020/src/App/NewTodoInput/index.tsx")
  (stop-tsparser))

(defun convert-to-pos (path pos)
  (defparameter *line-no* 0)
  (defparameter *result* 0)

  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
          while line
          when (= *line-no* (- (jsown:val pos "line") 1))
            ;;return (+ *result* (jsown:val pos "offset"))
            return (- (+ *result* (jsown:val pos "offset")) 1)
          do
            (setq *line-no* (+ *line-no* 1))
            ;; 改行コードも加算
            (setq *result* (+ *result* (+ (length line) 1)))
            ;;(setq *result* (+ *result* (length line)))
            ;;(format t "l=~a~a r=~a~%" *line-no* line *result*))))
            )))

(defun get-pos (item)
  (let ((name-span (jsown:val item "nameSpan")) start)
    (setq start (jsown:val name-span "start"))
    (cons :pos (cdr start))))

(defun find-item (tree line)
  (when (contains-line tree line)
      ;;(format t "found!! tree=~a%" tree)
      (setq *deepest-item* tree))

  (if (jsown:keyp tree "childItems")
      (progn
        ;;(format t "has child~%")
        (find-item (jsown:val tree "childItems") line))
      (when (jsown:keyp tree "obj")
        ;;(format t "has OBJ list~%")
        (loop for obj in tree do
              ;;(format t "objk=~a, text=~a, kind=~a~%"
              ;;        (jsown:keywords obj)
              ;;        (jsown:val obj "text")
              ;;        (jsown:val obj "kind"))
              (find-item obj line)))))

(defun contains-line (tree line)
  (when (and (jsown:keyp tree "text") (not (string= (jsown:val tree "kind") "module")))
    (let ((span (car (jsown:val tree "spans"))) start end)
      (setq start (jsown:val (jsown:val span "start") "line"))
      (setq end (jsown:val (jsown:val span "end") "line"))
      (and (>= line start) (>= end line)))))

(defun exec (command)
  (call command)
  (let ((stream (uiop:process-info-output *tsserver*)))
     ;; TODO: loop while (listen stream) do の方が安全？
       (loop while stream do
             (defvar result)
             (setq result (jsown:parse (extjson stream)))
             (when (string= (jsown:val result "type") "response")
               (return result)))))

(defun exec-tsparser (command)
  (call-tsparser command)
  (let ((stream (uiop:process-info-output *tsparser*)))
    ;;(loop while (listen stream) do
    ;;      (print "!!!!!")
    ;;     ;; Characters are immediately available
    ;;     (princ (read-line stream))
    ;;     (terpri)))
    (defvar result)
    ;;(setq result (jsown:parse (read-line stream)))
    (setq result (read-line stream))
    (format t "r=~a~%" result)
    (return result)
    )
  )

(defun call (command)
  (write-line command (uiop:process-info-input *tsserver*))
  (force-output (uiop:process-info-input *tsserver*)))

(defun call-tsparser (command)
  (write-line command (uiop:process-info-input *tsparser*))
  (force-output (uiop:process-info-input *tsparser*)))

(defun extjson (stream)
  ;; Content-Length: 99
  (read-line stream)
  ;; 改行コード
  (read-line stream)
  ;; JSON 
  (read-line stream))

