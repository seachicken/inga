(defpackage #:inga/parser/typescript
  (:use #:cl
        #:inga/parser/base
        #:inga/utils)
  (:import-from #:inga/cache
                #:put-value
                #:get-value)
  (:export #:parser-typescript))
(in-package #:inga/parser/typescript)

(defclass parser-typescript (parser)
  ((nearest-ast-pos :initform nil
                    :accessor parser-nearest-ast-pos)))

(defmethod make-parser ((kind (eql :typescript)) path cache)
  (make-instance 'parser-typescript :path path :cache cache))

(defmethod start-parser ((parser parser-typescript))
  (setf (parser-process parser)
        (uiop:launch-program "tsparser"
                             :input :stream :output :stream)))

(defmethod stop-parser ((parser parser-typescript))
  (uiop:close-streams (parser-process parser)))

(defmethod exec-parser ((parser parser-typescript) file-path)
  (let ((path (namestring
                (uiop:merge-pathnames* file-path (parser-path parser))))
        cache
        ast)
    (setf cache (get-value (parser-cache parser) (get-parse-key path)))
    (values
      (if cache
          (when (> (length cache) 0)
            (cdr (jsown:parse cache)))
          (progn
            (setf ast (exec-command parser path))
            (put-value (parser-cache parser) (get-parse-key path) ast)
            (when (> (length ast) 0)
              (cdr (jsown:parse ast)))))
      (when cache t))))

(defmethod find-affected-pos ((parser parser-typescript) file-path ast line-no)
  (let ((q (make-queue))
        (ast-pos (cdr (assoc :pos (convert-to-ast-pos
                                    (parser-path parser)
                                    (list
                                      (cons :path file-path)
                                      (cons :line line-no)
                                      (cons :offset -1)))))))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (and
                (jsown:keyp ast "kindName") (string= (jsown:val ast "kindName") "VariableDeclaration")
                (jsown:keyp ast "start") (<= (jsown:val ast "start") ast-pos)
                (jsown:keyp ast "end") (> (jsown:val ast "end") ast-pos))
          (let ((init (jsown:val ast "initializer")))
            (alexandria:switch ((jsown:val init "kindName") :test #'string=)
              ("ObjectLiteralExpression"
                (when (jsown:keyp ast "name")
                  (let ((name (cdr (jsown:val ast "name"))))
                    (return (convert-to-pos (parser-path parser) file-path
                                            (jsown:val name "escapedText")
                                            (jsown:val name "start"))))))
              ("CallExpression"
                (when (jsown:keyp ast "name")
                  (let ((name (cdr (jsown:val ast "name"))))
                    (alexandria:switch ((jsown:val name "kindName") :test #'string=)
                      ("ArrayBindingPattern"
                       (let ((elements (jsown:val name "elements")))
                         (return (convert-to-pos
                                   (parser-path parser) file-path
                                   (format nil "~{~a~^, ~}"
                                           (mapcar (lambda (e)
                                                     (when (jsown:keyp e "name")
                                                       (jsown:val (jsown:val e "name") "escapedText")))
                                                   elements))
                                   (jsown:val (first elements) "start")))))
                      ("Identifier"
                       (return (convert-to-pos (parser-path parser) file-path
                                               (jsown:val name "escapedText")
                                               (jsown:val name "start"))))))))
              ("ArrowFunction"
                (if (equal (find-return-type init) "JsxElement")
                    (enqueue q (jsown:val init "body"))
                    (return (convert-to-pos (parser-path parser) file-path
                                            (jsown:val (cdr (jsown:val ast "name")) "escapedText")
                                            (jsown:val ast "start"))))))))

        (when (and
                (jsown:keyp ast "kindName") (string= (jsown:val ast "kindName") "FunctionDeclaration")
                (jsown:keyp ast "start") (<= (jsown:val ast "start") ast-pos)
                (jsown:keyp ast "end") (> (jsown:val ast "end") ast-pos))
          (when (jsown:keyp ast "name")
            (let ((name (cdr (jsown:val ast "name"))))
              (return (convert-to-pos (parser-path parser) file-path
                                      (jsown:val name "escapedText")
                                      (jsown:val name "start"))))))

        (when (and
                (jsown:keyp ast "kindName") (string= (jsown:val ast "kindName") "MethodDeclaration")
                (jsown:keyp ast "start") (<= (jsown:val ast "start") ast-pos)
                (jsown:keyp ast "end") (> (jsown:val ast "end") ast-pos))
          (when (jsown:keyp ast "name")
            (let ((name (cdr (jsown:val ast "name"))))
              (return (convert-to-pos (parser-path parser) file-path
                                      (jsown:val name "escapedText")
                                      (jsown:val name "start"))))))

        (when (and
                (jsown:keyp ast "kindName") (string= (jsown:val ast "kindName") "FirstStatement"))
          (let ((dec-list (jsown:val (jsown:val ast "declarationList") "declarations")))
            (loop for d in dec-list do
                  (enqueue q (cdr d)))))

        (when (and
                (jsown:keyp ast "kindName") (string= (jsown:val ast "kindName") "ClassDeclaration"))
          (loop for m in (jsown:val ast "members") do
                (enqueue q (cdr m))))

        (when (jsown:keyp ast "statements")
          (loop for s in (jsown:val ast "statements") do
                (enqueue q (cdr s))))))))

(defmethod find-entrypoint ((parser parser-typescript) pos)
  (let ((ast-pos (convert-to-ast-pos (parser-path parser) pos)))
    (let ((path (cdr (assoc :path ast-pos)))
          (pos (cdr (assoc :pos ast-pos)))
          (ast (exec-parser parser (namestring (cdr (assoc :path ast-pos))))))
      (setf (parser-nearest-ast-pos parser) nil)
      (let ((component-pos (find-component parser ast pos)))
        (when component-pos
          (convert-to-pos (parser-path parser) path
                          (cdr (assoc :name component-pos))
                          (cdr (assoc :pos component-pos))))))))

(defun find-component (parser ast pos)
  (when (and (jsown:keyp ast "kindName")
             (or
               (string= (jsown:val ast "kindName") "JsxOpeningElement") 
               (string= (jsown:val ast "kindName") "JsxSelfClosingElement"))
             (jsown:keyp ast "tagName"))
    (setf (parser-nearest-ast-pos parser)
          (list
            (cons :name (let ((tag-name (cdr (jsown:val ast "tagName"))))
                          (if (jsown:keyp tag-name "escapedText")
                              (jsown:val tag-name "escapedText")
                              "")))
            (cons :pos (+ (jsown:val ast "start") 1)))))

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
          (jsown:keyp ast "type") (string= (jsown:val (jsown:val ast "type") "kindName") "VoidKeyword"))
    (return-from find-return-type))

  (let ((q (make-queue)))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (and (jsown:keyp ast "kindName") (string= (jsown:val ast "kindName") "ReturnStatement"))
          (when (and
                  (jsown:keyp ast "expression")
                  (string= (jsown:val (jsown:val ast "expression") "kindName") "ParenthesizedExpression"))
            (when 
              (string= (jsown:val (jsown:val
                                    (jsown:val ast "expression") "expression") "kindName") "JsxElement")
              (return "JsxElement"))))

        (when (jsown:keyp ast "statements")
          (loop for s in (jsown:val ast "statements") do
                (enqueue q (cdr s))))
        
        (when (jsown:keyp ast "body")
          (enqueue q (jsown:val ast "body")))))))

