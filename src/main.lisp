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
  (find-components (get-pos *deepest-item*)))

(defun find-components (pos)
  (defparameter src-path "/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/tests/fixtures/create-react-app-typescript-todo-example-2020/src/App/NewTodoInput/index.tsx")

  (print "open")
  (call (format nil "{\"seq\": 1, \"command\": \"open\", \"arguments\": {\"file\": \"~a\"}}" src-path))

  (print "references")
  (defparameter result
    (exec (format nil "{\"seq\": 2, \"command\": \"references\", \"arguments\": {\"file\": \"~a\", \"line\": ~a, \"offset\": ~a}}"
                  src-path (jsown:val pos "line") (jsown:val pos "offset"))))
  (defparameter body (jsown:val result "body"))
  (defparameter refs (jsown:val body "refs"))
  (defparameter refposs (remove-if (lambda (p) (equal p pos))
                                   (mapcar (lambda (r) (cons :pos (cdr (jsown:val r "start")))) refs)))
  (defparameter poss (mapcar (lambda (p) (convert-to-ast-pos src-path p)) refposs))
  (format t "poss=~a~%" poss)

  (start-tsparser)
  (defparameter ast
    (exec-tsparser src-path))
  (stop-tsparser) 

  (let ((json-cons-cells (cdr (jsown:parse ast)))) ;; パース結果のルートにあるcar "OBJ"を除く
    (mapcar (lambda (p)
              (convert-to-pos src-path (find-component json-cons-cells p))) poss))
)

(defparameter *jsx-opening-element* 276)
(defparameter *jsx-self-closing-element* 275)

(defparameter *nearest-comp-pos* nil)
(defparameter *result-nearest-comp-pos* nil)

(defun find-component (ast pos)
  (when (and (jsown:keyp ast "kind")
             (or
               (equal (jsown:val ast "kind") *jsx-opening-element*) 
               (equal (jsown:val ast "kind") *jsx-self-closing-element*)))
    ;;(format t "kind=~a, start=~a~%" (jsown:val ast "kind") (jsown:val ast "start"))
    ;; <の文字数を加算
    (setq *nearest-comp-pos* (+ (jsown:val ast "start") 1)))

  (when (not (null ast))
    (if (consp (car ast))
        (progn
          (when (and (jsown:keyp ast "start") (equal (jsown:val ast "start") pos))
            ;;(format t "start=~a, ast=~a~%" (jsown:val ast "start") ast)
            (return-from find-component *nearest-comp-pos*))

        ;; has children
        ;;(format t "cons t has children=~a, ~a~%" (consp (cdr (car ast))) ast)
        (if (consp (cdr (car ast)))
            (progn
              (let ((comp-pos (find-component (cdr (car ast)) pos)))
                (when (not (null comp-pos))
                  (return-from find-component comp-pos)))
              ;;(format t "else1~a~%" (cdr ast))
              (return-from find-component (find-component (cdr ast) pos)))
            (progn
              ;;(format t "else2~a~%" (cdr ast))
              (when (cdr ast)
                (return-from find-component (find-component (cdr ast) pos))))))
      (progn
        ;;(format t "else3~a~%" (cdr ast))
        (return-from find-component (find-component (cdr ast) pos))))))

(defun find-child-nodes (node)
  (if (jsown:keyp node "statements")
    (jsown:val node "statements")
  (when (jsown:keyp node "parameters")
    (jsown:val node "parameters"))))

(defun convert-to-ast-pos (path pos)
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

(defun convert-to-pos (path pos)
  (defparameter *line-no* 0)
  (defparameter cnt 0)

  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
          while line
          when (<= pos (+ cnt (length line) 1))
          return (cons :pos
                       (cons
                         (cons "line" (+ *line-no* 1))
                         (cons
                           (cons "offset" (- (+ (length line) 1) (- (+ cnt (length line)) pos)))
                           nil)))
          do
            (setq *line-no* (+ *line-no* 1))
            ;; 改行コードも加算
            (setq cnt (+ cnt (length line) 1))
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
      (when (jsown:keyp tree "OBJ")
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
    (read-line stream)
    ))
    ;;(defvar result)
    ;;;;(setq result (jsown:parse (read-line stream)))
    ;;(setq result (read-line stream))
    ;;;;(format t "r=~a~%" result)
    ;;(return result)))

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

