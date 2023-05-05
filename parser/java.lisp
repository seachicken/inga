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

(defmethod exec-parser ((parser parser-java) file-path)
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

(defmethod find-caller ((parser parser-java) index-path ast target-fq-name target-name)
  (let ((q (make-queue))
        (imported-name
          (concatenate 'string (car (last (split #\. target-fq-name))) "." target-name))
        (is-found-import (not (null (find target-fq-name (find-import-list (get-original-path index-path) ast) :test #'equal))))
        (root-ast ast)
        results)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (string= (cdar ast) "METHOD_INVOCATION")
          (let ((fq-name (get-fq-name parser (get-original-path index-path) root-ast ast nil)))
            (when (or
                    (and is-found-import (string= fq-name imported-name))
                    (string= fq-name target-fq-name))
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

(defmethod get-fq-name ((parser parser-java) src-path root-ast ast result)
  (when (string= (cdar ast) "MEMBER_SELECT")
    (setf result (concatenate 'string
                              (if result (concatenate 'string result ".") "")
                              (jsown:val (cdr ast) "name"))))

  (when (and
          result ;; already founded MEMBER_SELECT
          (string= (cdar ast) "IDENTIFIER"))
    (return-from get-fq-name
                 (find-declaration-for-identifier
                   root-ast
                   (when (jsown:keyp ast "name")
                     (let ((name (jsown:val ast "name")))
                       (convert-to-pos (parser-path parser) src-path
                                       name
                                       nil
                                       (jsown:val ast "pos"))))
                   (parser-path parser))))

  (loop for child in (jsown:val ast "children")
        do (setf result (get-fq-name parser src-path root-ast (cdr child) result)))
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
          ;;(setf result (format nil "~{~a~^.~}" (remove nil (list result (jsown:val ast "name")))))
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
                (jsown:keyp ast "name")
                (equal (jsown:val ast "name") target-name)
                (<= (jsown:val ast "startPos") ast-pos)
                (>= (jsown:val ast "endPos") ast-pos))
          (setf result (trace-declaration ast import-list target-name))
          (return-from find-declaration-for-identifier result))

        (loop for child in (jsown:val ast "children")
              do (let ((body (cdr child)))
                   (setf (jsown:val child "parent") ast)
                   (setf stack (append stack (list body)))))))))

(defun trace-declaration (ast import-list target-name)
  (let ((q (make-queue))
        results)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (equal (cdar ast) "CLASS")
          (loop for child in (jsown:val ast "children")
                do (when (and
                           (equal (jsown:val child "type") "VARIABLE")
                           (equal (jsown:val child "name") target-name))
                     (loop for child in (jsown:val child "children")
                           do (when (equal (jsown:val child "type") "IDENTIFIER")
                                (let ((class-name (jsown:val child "name")))
                                  (loop for import in import-list
                                        do (when (equal (car (last (split #\. import))) class-name)
                                             (setf results (append results (list import)))))))))))
        (when (jsown:keyp ast "parent")
          (enqueue q (jsown:val ast "parent")))))
    (format nil "~{~a~^.~}" results)))

