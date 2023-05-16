(defpackage #:inga/parser/java
  (:use #:cl
        #:inga/parser/base
        #:inga/utils)
  (:import-from #:inga/cache
                #:put-value
                #:get-value)
  (:import-from #:inga/parser/kotlin
                #:parser-kotlin)
  (:export #:parser-java))
(in-package #:inga/parser/java)

;; https://github.com/javaparser/javaparser/blob/d7a83612e1fa0c3c93ebac243a768339346382b5/javaparser-core/src/main/java/com/github/javaparser/JavaToken.java#L258
(defparameter *java-rbrace* 100) ;; }

(defclass parser-java (parser)
  ())

(defmethod make-parser ((kind (eql :java)) path cache)
  (list (make-instance 'parser-java :path path :cache cache)
        (make-instance 'parser-kotlin :path path :cache cache)))

(defmethod start-parser ((parser parser-java) include exclude)
  (setf (parser-process parser)
        (uiop:launch-program
          (format nil "~{~a~^ ~}"
                  (list "java" "-cp"
                        (format nil "~a/libs/javaparser.jar" (uiop:getenv "INGA_HOME"))
                        "--add-opens" "jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED"
                        "--add-opens" "jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED"
                        "--add-opens" "jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED"
                        "inga.Main"))
          :input :stream :output :stream))
  (create-indexes parser '("*.java") exclude))

(defmethod stop-parser ((parser parser-java))
  (clean-indexes)
  (uiop:close-streams (parser-process parser)))

(defmethod find-affected-pos ((parser parser-java) src-path ast line-no)
  (let ((q (make-queue))
        (top-offset (convert-to-top-offset (parser-path parser) src-path line-no -1))
        (root-ast ast)
        annotation-pos)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (and
                (or
                  (string= (cdar ast) "VARIABLE") 
                  (string= (cdar ast) "METHOD"))
                (<= (jsown:val ast "startPos") top-offset)
                (>= (jsown:val ast "endPos") top-offset))
          (when (jsown:keyp ast "name")
            (let ((name (jsown:val ast "name")))
              (let ((pos (convert-to-pos (parser-path parser) src-path
                                         name
                                         nil
                                         (jsown:val ast "pos"))))
                (push (cons :fq-name (get-fq-name-of-declaration root-ast pos (parser-path parser))) pos)
                (return pos)))))

        (when (and
                (string= (cdar ast) "com.github.javaparser.ast.body.ClassOrInterfaceDeclaration")
                (jsown:keyp ast "implementedTypes"))
          (let ((implementedTypes (jsown:val ast "implementedTypes")))
            (loop
              with implemented-type-name
              for implementedType in implementedTypes do
              (when (jsown:keyp implementedType "name")
                (setf implemented-type-name (cdr (jsown:val implementedType "name")))
                (when (string= (jsown:val implemented-type-name "identifier") "ConstraintValidator")
                  (let ((annotation (first (jsown:val implementedType "typeArguments")))
                        annotation-name)
                    (when (jsown:keyp annotation "name")
                      (setf annotation-name (jsown:val annotation "name"))
                      (setf annotation-pos
                            (list (cons :path src-path)
                                  (cons :name (jsown:val annotation-name "identifier"))
                                  (cons :line
                                        (jsown:val (jsown:val annotation-name "range") "beginLine"))
                                  (cons :offset
                                        (jsown:val (jsown:val annotation-name "range") "beginColumn")))))))))))

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
                    (= (jsown:val (jsown:val (jsown:val ast "tokenRange") "endToken") "kind") *java-rbrace*))
            (when (jsown:keyp ast "name")
              (let ((name (cdr (jsown:val ast "name"))))
                (if (and
                      annotation-pos
                      (string= (jsown:val name "identifier") "isValid"))
                    (return annotation-pos)
                    (return
                      (list (cons :path src-path)
                            (cons :name (jsown:val name "identifier"))
                            (cons :line (jsown:val (jsown:val name "range") "beginLine"))
                            (cons :offset (jsown:val (jsown:val name "range") "beginColumn")))))))
            (when (jsown:keyp ast "variables")
              (let ((name (cdr (jsown:val (cdr (first (jsown:val ast "variables"))) "name"))))
                (return
                  (list (cons :path src-path)
                        (cons :name (jsown:val name "identifier"))
                        (cons :line (jsown:val (jsown:val name "range") "beginLine"))
                        (cons :offset (jsown:val (jsown:val name "range") "beginColumn"))))))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children") do
                (enqueue q (cdr child))))))))

(defmethod find-entrypoint ((parser parser-java) pos))

(defmethod matches-reference-name ((parser parser-java) ast target-name)
  (and
    (or
      (equal (cdar ast) "MEMBER_SELECT") 
      (equal (cdar ast) "IDENTIFIER"))
    (equal (jsown:val ast "name") target-name)))

(defmethod find-reference-pos ((parser parser-java) index-path root-ast ast target-pos)
  (let ((q (make-queue))
        (target-name (cdr (assoc :name target-pos)))
        (reference-ast ast)
        result)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (equal (cdar ast) "MEMBER_SELECT")
          (loop for child in (jsown:val ast "children")
                do
                (when (equal (jsown:val child "type") "IDENTIFIER")
                  (setf result target-name)
                  (setf target-name (jsown:val child "name")))))

        (when (equal (cdar ast) "CLASS")
          (loop for child in (jsown:val ast "children")
                do
                (when (and
                        (equal (jsown:val child "type") "VARIABLE")
                        (equal (jsown:val child "name") target-name))
                  (loop for child in (jsown:val child "children")
                        do 
                        (when (equal (jsown:val child "type") "IDENTIFIER")
                          (let ((class-name (jsown:val child "name")))
                            (setf result (format nil "~{~a~^.~}"
                                                 (remove nil (list class-name result))))))))))

        (when (equal (cdar ast) "COMPILATION_UNIT")
          (loop for child in (jsown:val ast "children")
                with class-name = (first (split #\. result))
                do
                (when (and
                        (equal (jsown:val child "type") "IMPORT")
                        (equal (car (last (split #\. (jsown:val child "fqName")))) class-name))
                  (let ((split-import-names (split #\. (jsown:val child "fqName")))
                        fq-name)
                    (setf fq-name 
                          (format nil "~{~a~^.~}"
                                  (append (subseq split-import-names
                                                  0 
                                                  (1- (length split-import-names)))
                                          (list result))))
                    (when (equal fq-name (cdr (assoc :fq-name target-pos)))
                      (return-from
                        find-reference-pos
                        (convert-to-pos
                          (parser-path parser)
                          (get-original-path index-path)
                          nil
                          nil
                          (jsown:val reference-ast "pos"))))))))

        (when (jsown:keyp ast "parent")
          (enqueue q (jsown:val ast "parent")))))))

(defun get-fq-name-of-declaration (ast pos root-path)
  (let ((top-offset (convert-to-top-offset
                      root-path (cdr (assoc :path pos))
                      (cdr (assoc :line pos)) (cdr (assoc :offset pos))))
        (stack (list ast))
        result)
    (loop
      (let ((ast (pop stack)))
        (if (null ast) (return))

        (when (equal (cdar ast) "PACKAGE")
          (setf result (format nil "~{~a~^.~}" (remove nil (list result (jsown:val ast "packageName"))))))
        (when (or
                (equal (cdar ast) "CLASS")
                (equal (cdar ast) "INTERFACE"))
          (setf result (format nil "~{~a~^.~}" (remove nil (list result (jsown:val ast "name"))))))

        (when (and
                (jsown:keyp ast "name")
                (equal (jsown:val ast "name") (cdr (assoc :name pos)))
                (<= (jsown:val ast "startPos") top-offset)
                (>= (jsown:val ast "endPos") top-offset))
          (setf result (format nil "~{~a~^.~}" (remove nil (list result (jsown:val ast "name"))))) 
          (return-from get-fq-name-of-declaration result))

        (loop for child in (jsown:val ast "children")
              do (setf stack (append stack (list (cdr child)))))))))

