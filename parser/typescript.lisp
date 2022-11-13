(defpackage #:inga/parser/typescript
  (:use #:cl
        #:inga/parser/base
        #:inga/utils)
  (:export #:parser-typescript))
(in-package #:inga/parser/typescript)

(defclass parser-typescript (parser)
  ((nearest-ast-pos :initform nil
                    :accessor parser-nearest-ast-pos)))

(defmethod make-parser ((kind (eql :typescript)) path)
  (make-instance 'parser-typescript
                 :path path))

(defmethod start-parser ((parser parser-typescript))
  (setf (parser-process parser)
        (uiop:launch-program "tsparser"
                             :input :stream :output :stream)))

(defmethod stop-parser ((parser parser-typescript))
  (uiop:close-streams (parser-process parser)))

(defmethod exec-parser ((parser parser-typescript) file-path)
  (let ((ast (exec-command parser (namestring
                                    (uiop:merge-pathnames* file-path (parser-path parser))))))
    (when (> (length ast) 0)
      (cdr (jsown:parse ast)))))

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
                    (return (convert-to-pos (parser-path parser) file-path
                                            (jsown:val name "escapedText")
                                            (jsown:val name "start"))))))
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
          (when (string= (jsown:val (jsown:val ast "expression") "kindName") "ParenthesizedExpression")
            (when 
              (string= (jsown:val (jsown:val
                                    (jsown:val ast "expression") "expression") "kindName") "JsxElement")
              (return "JsxElement"))))

        (when (jsown:keyp ast "statements")
          (loop for s in (jsown:val ast "statements") do
                (enqueue q (cdr s))))
        
        (when (jsown:keyp ast "body")
          (enqueue q (jsown:val ast "body")))))))

(defun convert-to-ast-pos (project-path pos)
  (let ((path (uiop:merge-pathnames* (cdr (assoc :path pos)) project-path))
        (offset (cdr (assoc :offset pos)))
        (line-no 0)
        (result 0))
    (with-open-file (stream path)
      (loop for line = (read-line stream nil)
            while line
            when (= line-no (- (cdr (assoc :line pos)) 1))
            return (list
                     (cons :path (pathname path))
                     (cons :pos (+ result (if (< offset 0)
                                              (length line)
                                              (- offset 1)))))
            do
            (setq line-no (+ line-no 1))
            ;; add newline code
            (setq result (+ result (+ (length line) 1)))))))

(defun convert-to-pos (project-path path name pos)
  (let ((line-no 0)
        (cnt 0))
    (with-open-file (stream (uiop:merge-pathnames* path project-path))
      (loop for line = (read-line stream nil)
            while line
            when (<= pos (+ cnt (length line)))
            return (list
                     (cons :path (enough-namestring path project-path))
                     (cons :name name)
                     (cons :line (+ line-no 1))
                     (cons :offset (- (+ (length line) 1) (- (+ cnt (length line)) pos))))
            do
              (setq line-no (+ line-no 1))
              ;; add newline code
              (setq cnt (+ cnt (length line) 1))))))

