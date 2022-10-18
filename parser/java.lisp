(defpackage #:inga/parser/java
  (:use #:cl
        #:inga/parser/base
        #:inga/utils)
  (:export #:parser-java))
(in-package #:inga/parser/java)

(defparameter *java-rbrace** 100) ;; }

(defclass parser-java (parser)
  ())

(defmethod make-parser ((kind (eql :java)) path)
  (make-instance 'parser-java
                 :path path))

(defmethod start-parser ((parser parser-java))
  (setf (parser-process parser)
        (uiop:launch-program
          (format nil "java -cp ~a/libs/javaparser.jar inga.Main"
                  (uiop:getenv "INGA_HOME"))
          :input :stream :output :stream)))

(defmethod stop-parser ((parser parser-java))
  (uiop:close-streams (parser-process parser)))

(defmethod exec-parser ((parser parser-java) file-path)
  (let ((ast (exec-command parser (namestring
                                    (uiop:merge-pathnames* file-path (parser-path parser))))))
    (when (> (length ast) 0)
      (cdr (jsown:parse ast)))))

(defmethod find-affected-pos ((parser parser-java) src-path ast line-no)
  (let ((q (make-queue)))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (and
                (or
                  (string= (cdr (car ast)) "com.github.javaparser.ast.body.FieldDeclaration")
                  (string= (cdr (car ast)) "com.github.javaparser.ast.body.ConstructorDeclaration")
                  (string= (cdr (car ast)) "com.github.javaparser.ast.body.MethodDeclaration"))
                (<= (jsown:val (jsown:val ast "range") "beginLine") line-no)
                (>= (jsown:val (jsown:val ast "range") "endLine") line-no))
          (unless (and
                    (= (jsown:val (jsown:val ast "range") "endLine") line-no)
                    (jsown:keyp ast "tokenRange")
                    (= (jsown:val (jsown:val (jsown:val ast "tokenRange") "endToken") "kind") *java-rbrace**))
            (return
              (list (cons :name (if (jsown:keyp ast "name")
                                    (jsown:val (cdr (jsown:val ast "name")) "identifier")
                                    (if (jsown:keyp ast "variables")
                                        (jsown:val 
                                          (jsown:val (cdr (first (jsown:val ast "variables"))) "name")
                                          "identifier")
                                        "")))
                    (cons :line (jsown:val (jsown:val ast "range") "beginLine"))
                    (cons :offset (jsown:val (jsown:val ast "range") "beginColumn"))))))

        (when (jsown:keyp ast "types")
          (loop for type in (jsown:val ast "types") do
                (enqueue q (cdr type))))

        (when (jsown:keyp ast "members")
          (loop for member in (jsown:val ast "members") do
                (enqueue q (cdr member))))))))

(defmethod find-entrypoint ((parser parser-java) pos))

