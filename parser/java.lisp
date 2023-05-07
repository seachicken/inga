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
        (ast-pos (cdr (assoc :pos (convert-to-ast-pos
                                    (parser-path parser)
                                    (list
                                      (cons :path src-path)
                                      (cons :line line-no)
                                      (cons :offset -1)))))) 
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
                (<= (jsown:val ast "startPos") ast-pos)
                (>= (jsown:val ast "endPos") ast-pos))
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

(defmethod find-caller ((parser parser-java) index-path ast pos)
  (let ((q (make-queue))
        (target-fq-name (cdr (assoc :fq-name pos)))
        (target-name (cdr (assoc :name pos)))
        (root-ast ast)
        imported-name
        is-found-import
        results)
    (setf imported-name
          (concatenate 'string (car (last (split #\. target-fq-name))) "." target-name))
    (setf is-found-import (not (null (find target-fq-name (find-import-list (get-original-path index-path) ast) :test #'equal))))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (string= (cdar ast) "METHOD_INVOCATION")
          (let ((fq-method-name (find-fq-method-name parser (get-original-path index-path) root-ast ast nil)))
            (when (string= fq-method-name (concatenate 'string target-fq-name "." target-name))
              (setf results (append results
                                    (list
                                      (convert-to-pos
                                        (parser-path parser)
                                        (get-original-path index-path)
                                        nil
                                        nil
                                        (jsown:val ast "pos"))))))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children")
                do (enqueue q (cdr child))))))
    results))

(defmethod find-fq-method-name ((parser parser-java) src-path root-ast ast result)
  (let ((q (make-queue)))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (or
                (string= (cdar ast) "MEMBER_SELECT") 
                (string= (cdar ast) "IDENTIFIER"))
          (let ((found-declaration
                  (find-declaration-for-identifier
                    root-ast
                    (when (jsown:keyp ast "name")
                      (let ((pos (convert-to-pos (parser-path parser) src-path
                                                 (jsown:val ast "name")
                                                 nil
                                                 (jsown:val ast "pos"))))
                        (push (cons :fq-name (get-fq-name-of-declaration root-ast pos (parser-path parser))) pos)
                        pos))
                    (parser-path parser))))
            (return-from find-fq-method-name
                         (concatenate
                           'string
                           found-declaration
                           (when found-declaration ".")
                           (jsown:val ast "name")))))

        (loop for child in (jsown:val ast "children")
              do (enqueue q (cdr child))))))
  result)

(defun find-import-list (src-path ast)
  (let ((q (make-queue))
        results)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (string= (cdar ast) "IMPORT")
          (setf results (append results (list (jsown:val ast "fqName")))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children")
                do (enqueue q (cdr child))))))
    results))

(defun get-fq-name-of-declaration (ast pos root-path)
  (let ((ast-pos (cdr (assoc :pos (convert-to-ast-pos root-path pos))))
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
                (<= (jsown:val ast "startPos") ast-pos)
                (>= (jsown:val ast "endPos") ast-pos))
          (return-from get-fq-name-of-declaration result))

        (loop for child in (jsown:val ast "children")
              do (setf stack (append stack (list (cdr child)))))))))

(defun find-declaration-for-identifier (ast pos root-path)
  (let ((ast-pos (cdr (assoc :pos (convert-to-ast-pos root-path pos))))
        (stack (list ast))
        (import-list (find-import-list root-path ast))
        (target-name (cdr (assoc :name pos)))
        result)
    (loop
      (let ((ast (pop stack)))
        (if (null ast) (return))

        (when (and
                (or
                  (equal (cdar ast) "IDENTIFIER") 
                  (equal (cdar ast) "MEMBER_SELECT"))
                (jsown:keyp ast "name")
                (equal (jsown:val ast "name") target-name)
                (<= (jsown:val ast "startPos") ast-pos)
                (>= (jsown:val ast "endPos") ast-pos))
          (setf result (trace-declaration
                         (if (equal (cdar ast) "MEMBER_SELECT")
                             (let ((method-identifier-ast (first (jsown:val ast "children"))))
                               (setf (jsown:val method-identifier-ast "parent") ast)
                               (cdr method-identifier-ast))
                             ast)
                         import-list
                         pos))
          (return-from find-declaration-for-identifier result))

        (loop for child in (jsown:val ast "children")
              do (let ((body (cdr child)))
                   (setf (jsown:val child "parent") ast)
                   (setf stack (append stack (list body)))))))))

(defun trace-declaration (ast import-list target-pos)
  (let ((q (make-queue))
        (target-name (cdr (assoc :name target-pos)))
        found-fq-name)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))
                     
        (when (and
                (equal (cdar ast) "IDENTIFIER")
                (equal (cdar (jsown:val ast "parent")) "MEMBER_SELECT")
                (equal (jsown:val (jsown:val ast "parent") "name") target-name))
          (let ((class-name (jsown:val ast "name")))
            (setf target-name class-name)))

        (when (equal (cdar ast) "CLASS")
          (loop for child in (jsown:val ast "children")
                do (progn
                     (when (and
                             (equal (jsown:val child "type") "VARIABLE")
                             (equal (jsown:val child "name") target-name))
                       (loop for child in (jsown:val child "children")
                             do (when (equal (jsown:val child "type") "IDENTIFIER")
                                  (let ((class-name (jsown:val child "name")))
                                    (loop for import in import-list
                                          do (when (equal (car (last (split #\. import))) class-name)
                                               (return-from trace-declaration import)))))))
                     (when (and
                             (equal (jsown:val child "type") "METHOD")
                             (equal (jsown:val child "name") target-name))
                       (return-from trace-declaration (cdr (assoc :fq-name target-pos)))))))
        (when (jsown:keyp ast "parent")
          (enqueue q (jsown:val ast "parent")))))))

