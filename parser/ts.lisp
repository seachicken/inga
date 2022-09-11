(defpackage #:inga/parser/ts
  (:use #:cl
        #:inga/parser/base
        #:inga/ts-helper
        #:inga/utils)
  (:export #:parser-ts))
(in-package #:inga/parser/ts)

(defparameter *void-keyword* 113)
(defparameter *parenthesized-expression* 208)
(defparameter *arrow-function* 210)
(defparameter *variable-statement* 233)
(defparameter *return-statement* 243)
(defparameter *variable-declaration* 250)
(defparameter *function-declaration* 252)
(defparameter *jsx-element* 274)
(defparameter *jsx-self-closing-element* 275)
(defparameter *jsx-opening-element* 276)

(defclass parser-ts (parser)
  ((nearest-ast-pos :initform nil
                    :accessor parser-nearest-ast-pos)))

(defmethod make-parser ((kind (eql :ts)))
  (make-instance 'parser-ts))

(defmethod start-parser ((parser parser-ts))
  (setf (parser-process parser)
        (uiop:launch-program "tsparser"
                             :input :stream :output :stream)))

(defmethod stop-parser ((parser parser-ts))
  (uiop:close-streams (parser-process parser)))

(defmethod exec-parser ((parser parser-ts) file-path)
  (let ((ast (exec-command parser file-path)))
    (when (> (length ast) 0)
      (cdr (jsown:parse ast)))))

(defmethod find-affected-pos ((parser parser-ts) project-path file-path ast line-no)
  (let ((q (make-queue))
        (ast-pos (cdr (assoc :pos (convert-to-ast-pos
                                (list
                                  (cons :path file-path)
                                  (cons :line line-no)
                                  (cons :offset 0)))))))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (and
                (jsown:keyp ast "kind") (= (jsown:val ast "kind") *variable-declaration*)
                (jsown:keyp ast "start") (<= (jsown:val ast "start") ast-pos)
                (jsown:keyp ast "end") (> (jsown:val ast "end") ast-pos))
          (let ((init (jsown:val ast "initializer")))
            (when (= (jsown:val init "kind") *arrow-function*)
              (if (equal (find-return-type init) *jsx-element*)
                  (enqueue q (jsown:val init "body"))
                  (return (convert-to-pos project-path file-path (jsown:val ast "start")))))))

        (when (and
                (jsown:keyp ast "kind") (= (jsown:val ast "kind") *function-declaration*)
                (jsown:keyp ast "start") (<= (jsown:val ast "start") ast-pos)
                (jsown:keyp ast "end") (> (jsown:val ast "end") ast-pos))
          (return (convert-to-pos project-path file-path (jsown:val ast "start"))))

        (when (and
                (jsown:keyp ast "kind")
                (= (jsown:val ast "kind") *variable-statement*))
          (let ((dec-list (jsown:val (jsown:val ast "declarationList") "declarations")))
            (loop for d in dec-list do
                  (enqueue q (cdr d)))))

        (when (jsown:keyp ast "statements")
          (loop for s in (jsown:val ast "statements") do
                (enqueue q (cdr s))))))))

(defmethod find-entrypoint ((parser parser-ts) project-path pos)
  (let ((ast-pos (convert-to-ast-pos pos)))
    (let ((path (cdr (assoc :path ast-pos)))
          (pos (cdr (assoc :pos ast-pos)))
          (ast (exec-parser parser (namestring (cdr (assoc :path ast-pos))))))
      (setf (parser-nearest-ast-pos parser) nil)
      (let ((pos (find-component parser ast pos)))
        (when pos
          (convert-to-pos project-path path pos))))))

(defun find-component (parser ast pos)
  (when (and (jsown:keyp ast "kind")
             (or
               (equal (jsown:val ast "kind") *jsx-opening-element*) 
               (equal (jsown:val ast "kind") *jsx-self-closing-element*)))
    (setf (parser-nearest-ast-pos parser) (+ (jsown:val ast "start") 1)))

  (when (not (null ast))
    (if (consp (car ast))
        (progn
          (when (and (jsown:keyp ast "start") (equal (jsown:val ast "start") pos))
            (return-from find-component (parser-nearest-ast-pos parser)))

        (if (consp (cdr (car ast)))
            (progn
              (let ((comp-pos (find-component parser (cdr (car ast)) pos)))
                (when (not (null comp-pos))
                  (return-from find-component comp-pos)))
              (return-from find-component (find-component parser (cdr ast) pos)))
            (progn
              (when (cdr ast)
                (return-from find-component (find-component parser (cdr ast) pos))))))
      (progn
        (return-from find-component (find-component parser (cdr ast) pos))))))

(defun find-return-type (ast)
  (when (and
          (jsown:keyp ast "type")
          (= (jsown:val (jsown:val ast "type") "kind") *void-keyword*))
    (return-from find-return-type))

  (let ((q (make-queue)))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (and (jsown:keyp ast "kind") (= (jsown:val ast "kind") *return-statement*))
          (when (= (jsown:val (jsown:val ast "expression") "kind") *parenthesized-expression*)
            (when 
              (= (jsown:val (jsown:val
                              (jsown:val ast "expression") "expression") "kind") *jsx-element*)
              (return *jsx-element*))))

        (when (jsown:keyp ast "statements")
          (loop for s in (jsown:val ast "statements") do
                (enqueue q (cdr s))))
        
        (when (jsown:keyp ast "body")
          (enqueue q (jsown:val ast "body")))))))

