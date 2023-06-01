(defpackage #:inga/ast-analyzer/java
  (:use #:cl
        #:inga/ast-analyzer/base
        #:inga/utils)
  (:import-from #:inga/cache
                #:put-value
                #:get-value)
  (:import-from #:inga/ast-analyzer/kotlin
                #:ast-analyzer-kotlin)
  (:export #:ast-analyzer-java))
(in-package #:inga/ast-analyzer/java)

;; https://github.com/javaparser/javaparser/blob/d7a83612e1fa0c3c93ebac243a768339346382b5/javaparser-core/src/main/java/com/github/javaparser/JavaToken.java#L258
(defparameter *java-rbrace* 100) ;; }

(defclass ast-analyzer-java (ast-analyzer)
  ())

(defmethod make-ast-analyzer ((kind (eql :java)) path cache)
  (list (make-instance 'ast-analyzer-java :path path :cache cache)
        (make-instance 'ast-analyzer-kotlin :path path :cache cache)))

(defmethod start-ast-analyzer ((ast-analyzer ast-analyzer-java) include exclude)
  (setf (ast-analyzer-process ast-analyzer)
        (uiop:launch-program
          (format nil "~{~a~^ ~}"
                  (list "java" "-cp"
                        (format nil "~a/libs/javaparser.jar" (uiop:getenv "INGA_HOME"))
                        "--add-opens" "jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED"
                        "--add-opens" "jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED"
                        "--add-opens" "jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED"
                        "inga.Main"))
          :input :stream :output :stream))
  (create-indexes ast-analyzer '("*.java") exclude))

(defmethod stop-ast-analyzer ((ast-analyzer ast-analyzer-java))
  (clean-indexes)
  (uiop:close-streams (ast-analyzer-process ast-analyzer)))

(defmethod find-definitions ((ast-analyzer ast-analyzer-java) range)
  (let ((q (make-queue))
        (src-path (cdr (assoc :path range)))
        (index-path (get-index-path (cdr (assoc :path range))))
        (start-offset (cdr (assoc :start-offset range)))
        (end-offset (cdr (assoc :end-offset range)))
        ast
        root-ast
        annotation-pos
        results)
    (setf ast (cdr (jsown:parse (uiop:read-file-string index-path))))
    (setf root-ast ast)
    (enqueue q ast)
    (loop
      (setf ast (dequeue q))
      (if (null ast) (return))

      (when (and
              (or
                ;; field reference
                (and
                  (jsown:keyp ast "parent")
                  (equal (cdar (jsown:val ast "parent")) "CLASS") 
                  (equal (cdar ast) "VARIABLE"))
                (and
                  (equal (cdar ast) "METHOD")
                  ;; TODO: support constructor reference
                  (not (equal (jsown:val ast "name") "<init>"))))
              (contains-offset (jsown:val ast "startPos") (jsown:val ast "endPos")
                               start-offset end-offset))
        (when (jsown:keyp ast "name")
          (setf results
                (append results
                        (list
                          (let ((pos (list
                                       (cons :path src-path)
                                       (cons :name (jsown:val ast "name"))
                                       (cons :fq-name (get-fq-name-of-declaration
                                                        root-ast (ast-analyzer-path ast-analyzer)
                                                        (jsown:val ast "name") (jsown:val ast "pos")))
                                       (cons :top-offset (jsown:val ast "pos")))))
                            (when (assoc :origin range)
                              (push (cons :origin (cdr (assoc :origin range))) pos))
                            pos))))))

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

      (loop for child in (jsown:val ast "children")
            do
            (let ((body (cdr child)))
              (setf (jsown:val child "parent") ast)
              (enqueue q (cdr child)))))
    results))

(defmethod find-entrypoint ((ast-analyzer ast-analyzer-java) pos))

(defmethod matches-reference-name ((ast-analyzer ast-analyzer-java) ast target-name)
  (and
    (or
      (equal (cdar ast) "MEMBER_SELECT") 
      (equal (cdar ast) "IDENTIFIER"))
    (equal (jsown:val ast "name") target-name)))

(defmethod find-reference-pos ((ast-analyzer ast-analyzer-java) index-path root-ast ast target-pos)
  (let ((q (make-queue))
        (target-name (cdr (assoc :name target-pos)))
        target-obj
        (reference-ast ast)
        result-pos
        result)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (equal (cdar ast) "IDENTIFIER")
          (setf result target-name) 
          (setf result-pos (jsown:val ast "pos")))

        (when (equal (cdar ast) "MEMBER_SELECT")
          (loop for child in (jsown:val ast "children")
                do
                (when (equal (jsown:val child "type") "IDENTIFIER")
                  (setf result target-name)
                  (setf target-obj (jsown:val child "name")))))

        (when (equal (cdar ast) "METHOD")
          (loop for child in (jsown:val ast "children")
                do
                (when (and
                        (equal (jsown:val child "type") "VARIABLE")
                        (equal (jsown:val child "name") target-name))
                  (return-from find-reference-pos))
                (when (and
                        (equal (jsown:val child "type") "RETURN")
                        (find target-name (jsown:val child "children")))
                  (return-from find-reference-pos))))

        (when (equal (cdar ast) "CLASS")
          (loop for child in (jsown:val ast "children")
                do
                (when (and
                        (equal (jsown:val child "type") "VARIABLE")
                        (equal (jsown:val child "name") target-obj))
                  (loop for child in (jsown:val child "children")
                        do 
                        (when (equal (jsown:val child "type") "IDENTIFIER")
                          (let ((class-name (jsown:val child "name")))
                            (setf result (format nil "~{~a~^.~}"
                                                 (remove nil (list class-name result))))))))
                (when (and
                        result-pos
                        (null target-obj)
                        (equal (jsown:val child "type") "METHOD")
                        (equal (jsown:val child "name") target-name))
                  (let ((fq-name (get-fq-name-of-declaration root-ast
                                                             (get-original-path index-path)
                                                             target-name
                                                             result-pos)))
                    (when (equal fq-name (cdr (assoc :fq-name target-pos)))
                      (return-from
                        find-reference-pos
                        (list
                          (cons :path (get-original-path index-path))
                          (cons :top-offset result-pos))))))))

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
                        (list
                          (cons :path (get-original-path index-path))
                          (cons :top-offset (jsown:val reference-ast "pos")))))))))

        (when (jsown:keyp ast "parent")
          (enqueue q (jsown:val ast "parent")))))))

(defun get-fq-name-of-declaration (ast root-path name top-offset)
  (let ((stack (list ast))
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
                (equal (jsown:val ast "name") name)
                (<= (jsown:val ast "startPos") top-offset)
                (>= (jsown:val ast "endPos") top-offset))
          (setf result (format nil "~{~a~^.~}" (remove nil (list result (jsown:val ast "name"))))) 
          (return-from get-fq-name-of-declaration result))

        (loop for child in (jsown:val ast "children")
              do (setf stack (append stack (list (cdr child)))))))))

