(defpackage :inga/main
  (:use :cl
        :inga/git
        :inga/ts-helper)
  (:import-from :jsown)
  (:export #:start
           #:stop
           #:analyze
           #:find-components))
(in-package :inga/main)

(defvar *deepest-item*)
(defvar *tsserver*)
(defvar *tsparser*)

(defun start ()
  (print "hello")
  (setq *tsserver* (uiop:launch-program "tsserver" :input :stream :output :stream)))

(defun stop ()
  (print "bye")
  (uiop:close-streams *tsserver*))

(defun start-tsparser ()
  (print "start tsparser")
  (setq *tsparser* (uiop:launch-program "tsparser" :input :stream :output :stream)))

(defun stop-tsparser ()
  (print "stop tsparser")
  (uiop:close-streams *tsparser*))

(defun get-val (list key)
  (loop for item in list do
        (when (equal (car item) key)
          (return-from get-val (cdr item)))))

(defun analyze (project-path sha-a &optional (sha-b))
  (mapcan (lambda (range)
            ;;(format t "r=~a~%" range)
            (defparameter src-path (format nil "~a~a" project-path (get-val range "path")))

            (print "open")
            (call (format nil "{\"seq\": 1, \"command\": \"open\", \"arguments\": {\"file\": \"~a\"}}" src-path))

            (print "navtree")
            (defvar result)
            (setq result (exec (format nil "{\"seq\": 2, \"command\": \"navtree\", \"arguments\": {\"file\": \"~a\"}}" src-path)))
            (defparameter body (jsown:val result "body"))
            ;;(format t "body=~a~%" body)
            (mapcan (lambda (line)
                      (find-item body line)
                      (when (get-pos *deepest-item*)
                        (find-components src-path (get-pos *deepest-item*))))
                    (loop for line from (get-val range "start") to (get-val range "end") collect line)))
          (get-diff project-path sha-a sha-b))
  )

(defun find-components (src-path pos)
  ;;(format t "find-components p=~a, pos=~a~%" src-path pos)
  (print "open")
  (call (format nil "{\"seq\": 1, \"command\": \"open\", \"arguments\": {\"file\": \"~a\"}}" src-path))

  (print "references")
  (defparameter result
    (exec (format nil "{\"seq\": 2, \"command\": \"references\", \"arguments\": {\"file\": \"~a\", \"line\": ~a, \"offset\": ~a}}"
                  src-path (jsown:val pos "line") (jsown:val pos "offset"))))
  ;;(format t "~%refs=~a" result)
  (defparameter body (jsown:val result "body"))
  (defparameter refs (jsown:val body "refs"))
  ;;(format t "refs=~a~%" refs)
  (defparameter refposs (remove-if (lambda (p) (equal p pos))
                                   ;;(mapcar (lambda (r) (cons :pos (cdr (jsown:val r "start")))) refs)))
                                   (mapcar (lambda (r)
                                             (cons :pos
                                                   ;; TODO: mapの方が後で参照できそう
                                                   (list
                                                     (cons "path" (jsown:val r "file"))
                                                     (nth 0 (cdr (jsown:val r "start")))
                                                     (nth 1 (cdr (jsown:val r "start"))))))
                                           refs)))
  (format t "refposs=~a~%" refposs)
  (defparameter poss (mapcar (lambda (p)
                               (let ((path (cdr (nth 0 (cdr p)))))
                                 (list
                                   path
                                   (convert-to-ast-pos path p))))
                             refposs))
  ;;(format t "poss=~a~%" poss)

  (start-tsparser)
  (defparameter ast
    (exec-tsparser src-path))
  (stop-tsparser) 

  (let ((json-cons-cells (cdr (jsown:parse ast)))) ;; パース結果のルートにあるcar "OBJ"を除く
    (mapcar (lambda (p)
              (let ((path (nth 0 p)) (pos (nth 1 p)))
                ;;(format t "path=~a, pos=~a~%" path pos)
                (defparameter comp-pos (find-component json-cons-cells pos))
                (when (not (null comp-pos))
                  (convert-to-pos path comp-pos))))
            poss))
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

(defun find-item (tree line)
  (when (contains-line tree line)
      ;;(format t "found!! line=~a, tree=~a%" line tree)
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

