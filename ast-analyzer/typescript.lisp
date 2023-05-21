(defpackage #:inga/ast-analyzer/typescript
  (:use #:cl
        #:inga/ast-analyzer/base
        #:inga/utils)
  (:import-from #:inga/cache
                #:put-value
                #:get-value)
  (:export #:ast-analyzer-typescript))
(in-package #:inga/ast-analyzer/typescript)

(defclass ast-analyzer-typescript (ast-analyzer)
  ((nearest-ast-pos :initform nil
                    :accessor ast-analyzer-nearest-ast-pos)))

(defmethod make-ast-analyzer ((kind (eql :typescript)) path cache)
  (make-instance 'ast-analyzer-typescript :path path :cache cache))

(defmethod start-ast-analyzer ((ast-analyzer ast-analyzer-typescript) include exclude)
  (setf (ast-analyzer-process ast-analyzer)
        (uiop:launch-program "tsparser"
                             :input :stream :output :stream))
  (create-indexes ast-analyzer include exclude))

(defmethod stop-ast-analyzer ((ast-analyzer ast-analyzer-typescript))
  (clean-indexes)
  (uiop:close-streams (ast-analyzer-process ast-analyzer)))

(defmethod find-affected-poss ((ast-analyzer ast-analyzer-typescript) range)
  (let ((q (make-queue))
        (src-path (cdr (assoc :path range)))
        (index-path (get-index-path (cdr (assoc :path range))))
        (start-offset (cdr (assoc :start-offset range)))
        (end-offset (cdr (assoc :end-offset range)))
        ast
        results)
    (setf ast (cdr (jsown:parse (uiop:read-file-string index-path))))
    (enqueue q ast)
    (loop
      (setf ast (dequeue q))
      (if (null ast) (return))

      (when (and
              (jsown:keyp ast "kindName") (string= (jsown:val ast "kindName") "VariableDeclaration")
              (contains-offset (jsown:val ast "start") (jsown:val ast "end")
                               start-offset end-offset))
        (let ((init (jsown:val ast "initializer")))
          (alexandria:switch ((jsown:val init "kindName") :test #'string=)
            ("ObjectLiteralExpression"
              (when (jsown:keyp ast "name")
                (let ((name (cdr (jsown:val ast "name"))))
                  (setf results
                        (append results
                                (list
                                  (let ((pos (list
                                               (cons :path src-path)
                                               (cons :name (jsown:val name "escapedText"))
                                               (cons :top-offset (jsown:val ast "start")))))
                                    (when (assoc :origin range)
                                      (push (cons :origin (cdr (assoc :origin range))) pos))
                                    pos)))))))
            ("CallExpression"
              (when (jsown:keyp ast "name")
                (let ((name (cdr (jsown:val ast "name"))))
                  (alexandria:switch ((jsown:val name "kindName") :test #'string=)
                    ("ArrayBindingPattern"
                     (let ((elements (jsown:val name "elements")))
                       (setf results
                             (append results
                                     (list
                                       (let ((pos (list
                                                    (cons :path src-path)
                                                    (cons :name 
                                                          (format nil "~{~a~^, ~}"
                                                                  (mapcar (lambda (e)
                                                                            (when (jsown:keyp e "name")
                                                                              (jsown:val (jsown:val e "name") "escapedText")))
                                                                          elements)))
                                                    (cons :top-offset (jsown:val (first elements) "start")))))
                                         (when (assoc :origin range)
                                           (push (cons :origin (cdr (assoc :origin range))) pos))
                                         pos))))))
                    ("Identifier"
                      (setf results
                            (append results
                                    (list
                                      (let ((pos (list
                                                   (cons :path src-path)
                                                   (cons :name (jsown:val name "escapedText"))
                                                   (cons :top-offset (jsown:val name "start")))))
                                        (when (assoc :origin range)
                                          (push (cons :origin (cdr (assoc :origin range))) pos))
                                        pos)))))))))
            ("ArrowFunction"
              (if (equal (find-return-type init) "JsxElement")
                  (enqueue q (jsown:val init "body"))
                  (setf results
                        (append results
                                (list
                                  (let ((pos (list
                                               (cons :path src-path)
                                               (cons :name (jsown:val (cdr (jsown:val ast "name")) "escapedText"))
                                               (cons :top-offset (jsown:val ast "start")))))
                                    (when (assoc :origin range)
                                      (push (cons :origin (cdr (assoc :origin range))) pos))
                                    pos)))))))))
      (when (and
              (jsown:keyp ast "kindName") (string= (jsown:val ast "kindName") "FunctionDeclaration")
              (contains-offset (jsown:val ast "start") (jsown:val ast "end")
                               start-offset end-offset))
        (when (jsown:keyp ast "name")
          (let ((name (cdr (jsown:val ast "name"))))
            (setf results
                  (append results
                          (list
                            (let ((pos (list
                                         (cons :path src-path)
                                         (cons :name (jsown:val name "escapedText"))
                                         (cons :top-offset (jsown:val name "start")))))
                              (when (assoc :origin range)
                                (push (cons :origin (cdr (assoc :origin range))) pos))
                              pos)))))))
        (when (and
                (jsown:keyp ast "kindName") (string= (jsown:val ast "kindName") "MethodDeclaration")
                (contains-offset (jsown:val ast "start") (jsown:val ast "end")
                                 start-offset end-offset))
          (when (jsown:keyp ast "name")
            (let ((name (cdr (jsown:val ast "name"))))
              (setf results
                    (append results
                            (list
                              (let ((pos (list
                                           (cons :path src-path)
                                           (cons :name (jsown:val name "escapedText"))
                                           (cons :top-offset (jsown:val name "start")))))
                                (when (assoc :origin range)
                                  (push (cons :origin (cdr (assoc :origin range))) pos))
                                pos)))))))
        (when (and
                (jsown:keyp ast "kindName") (string= (jsown:val ast "kindName") "FirstStatement"))
          (let ((dec-list (jsown:val (jsown:val ast "declarationList") "declarations")))
            (loop for d in dec-list
                  do (enqueue q (cdr d)))))

        (when (and
                (jsown:keyp ast "kindName") (string= (jsown:val ast "kindName") "ClassDeclaration"))
          (loop for m in (jsown:val ast "members")
                do (enqueue q (cdr m))))

        (when (jsown:keyp ast "statements")
          (loop for s in (jsown:val ast "statements")
                do (enqueue q (cdr s)))))
    results))

(defmethod find-entrypoint ((ast-analyzer ast-analyzer-typescript) pos)
  (let ((top-offset (cdr (assoc :top-offset pos)))
        (ast (cdr (jsown:parse (uiop:read-file-string (get-index-path (cdr (assoc :path pos))))))))
    (setf (ast-analyzer-nearest-ast-pos ast-analyzer) nil)
    (let ((component-pos (find-component ast-analyzer ast top-offset)))
      (when component-pos
        (let ((result (list
                     (cons :path (cdr (assoc :path pos)))
                     (cons :name (cdr (assoc :name component-pos)))
                     (cons :top-offset (cdr (assoc :pos component-pos))))))
          (when (assoc :origin pos)
            (push (cons :origin (cdr (assoc :origin pos))) result))
          result)))))

(defmethod find-references ((ast-analyzer ast-analyzer-typescript) pos))

(defun find-component (ast-analyzer ast pos)
  (when (and (jsown:keyp ast "kindName")
             (or
               (string= (jsown:val ast "kindName") "JsxOpeningElement") 
               (string= (jsown:val ast "kindName") "JsxSelfClosingElement"))
             (jsown:keyp ast "tagName"))
    (setf (ast-analyzer-nearest-ast-pos ast-analyzer)
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
            (return-from find-component (ast-analyzer-nearest-ast-pos ast-analyzer)))

        (if (consp (cdr (car ast)))
            (progn
              (let ((comp-pos (find-component ast-analyzer (cdr (car ast)) pos)))
                (when (not (null comp-pos))
                  (return-from find-component comp-pos)))
              (return-from find-component (find-component ast-analyzer (cdr ast) pos)))
            (progn
              (when (cdr ast)
                (return-from find-component (find-component ast-analyzer (cdr ast) pos))))))
      (progn
        (return-from find-component (find-component ast-analyzer (cdr ast) pos))))))

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
          (loop for s in (jsown:val ast "statements")
                do (enqueue q (cdr s))))
        
        (when (jsown:keyp ast "body")
          (enqueue q (jsown:val ast "body")))))))

