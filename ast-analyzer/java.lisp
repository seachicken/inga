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
    (handler-case
      (setf ast (cdr (jsown:parse (uiop:read-file-string index-path))))
      (error (e)
             (format t "~a~%" e)
             (return-from find-definitions)))
    (setf root-ast ast)
    (enqueue q ast)
    (loop
      with class-name
      do
      (setf ast (dequeue q))
      (if (null ast) (return))

      (when (equal (cdar ast) "CLASS")
        (setf class-name (jsown:val ast "name")))
      (when (and
              (or
                ;; field reference
                (and
                  (jsown:keyp ast "parent")
                  (equal (cdar (jsown:val ast "parent")) "CLASS") 
                  (equal (cdar ast) "VARIABLE"))
                  (equal (cdar ast) "METHOD"))
              (contains-offset (jsown:val ast "startPos") (jsown:val ast "endPos")
                               start-offset end-offset))
        (when (jsown:keyp ast "name")
          (setf results
                (append results
                        (list
                          (let ((pos (list
                                       (cons :path src-path)
                                       (if (equal (jsown:val ast "name") "<init>")
                                           (cons :name class-name)
                                           (cons :name (jsown:val ast "name")))
                                       (cons :fq-name (get-fq-name-of-declaration
                                                        root-ast
                                                        (jsown:val ast "pos")))
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
  (alexandria:switch ((cdar ast) :test #'equal)
    ("NEW_CLASS"
      (equal (jsown:val ast "name") target-name))
    ("METHOD_INVOCATION"
      (loop for child in (jsown:val ast "children")
            do
            (when (equal (jsown:val child "type") "IDENTIFIER")
              (return (equal (jsown:val child "name") target-name)))  
            (when (equal (jsown:val child "type") "MEMBER_SELECT")
              (return (equal (jsown:val child "name") target-name)))))))

(defmethod find-reference-pos ((ast-analyzer ast-analyzer-java) index-path root-ast ast target-pos)
  (let ((q (make-queue))
        (target-name (cdr (assoc :name target-pos)))
        target-obj
        (reference-ast ast)
        class-name
        params
        name-with-params
        result-pos)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (and
                (eq (jsown:val ast "pos") (jsown:val reference-ast "pos"))
                (or (equal (cdar ast) "NEW_CLASS")
                    (equal (cdar ast) "METHOD_INVOCATION")))
          (let (has-set-name)
            (when (equal (cdar ast) "NEW_CLASS")
              (setf class-name (jsown:val ast "name"))
              (setf result-pos (jsown:val ast "pos")) 
              (setf name-with-params (jsown:val ast "name"))
              (setf has-set-name t))
            (loop for child in (jsown:val ast "children")
                  with placeholder-i = 0
                  do
                  (when (jsown:keyp child "name")
                    (if has-set-name
                        (if (or
                              (uiop:string-suffix-p (jsown:val child "type") "_LITERAL")
                              (equal (jsown:val child "type") "NEW_CLASS"))
                            (setf name-with-params
                                  (concatenate 'string name-with-params "-"
                                               (if (equal (jsown:val child "type") "STRING_LITERAL")
                                                   "String"
                                                   (jsown:val child "name"))))
                            (progn
                              (setf params (append params (list (jsown:val child "name"))))
                              (setf name-with-params (format nil "~{~a~^-%~}"
                                                             (remove nil (list name-with-params placeholder-i))))  
                              (setf placeholder-i (1+ placeholder-i))))
                        (setf name-with-params (jsown:val child "name")))
                    (setf has-set-name t))
                  (when (equal (jsown:val child "type") "IDENTIFIER")
                    (setf result-pos (jsown:val child "pos")))
                  (when (equal (jsown:val child "type") "MEMBER_SELECT")
                    (setf result-pos (jsown:val child "pos")) 
                    (loop for child in (jsown:val child "children")
                          do
                          (when (equal (jsown:val child "type") "IDENTIFIER")
                            (setf target-obj (jsown:val child "name")))
                          (when (equal (jsown:val child "type") "NEW_CLASS")
                            (setf class-name (jsown:val child "name"))))))))

        (when (or
                (equal (cdar ast) "METHOD")
                (equal (cdar ast) "BLOCK")
                (equal (cdar ast) "TRY"))
          (loop for child in (jsown:val ast "children")
                do
                (when (equal (jsown:val child "type") "VARIABLE")
                  (let ((found-param-i
                          (position (jsown:val child "name") params :test #'equal)))
                    (when found-param-i
                      (loop for child in (jsown:val child "children")
                            do
                            (when (jsown:keyp child "name")
                              (setf name-with-params
                                    (ppcre:regex-replace-all
                                      (format nil "%~a" found-param-i)
                                      name-with-params
                                      (jsown:val child "name"))))))))))

        (when (equal (cdar ast) "CLASS")
          (loop for child in (jsown:val ast "children")
                do
                (when (and
                        (equal (jsown:val child "type") "VARIABLE")
                        (or
                          (equal (jsown:val child "name") target-obj)
                          (find (jsown:val child "name") params)))
                  (loop for child in (jsown:val child "children")
                        do 
                        (when (equal (jsown:val child "type") "IDENTIFIER")
                          (setf class-name (jsown:val child "name")))))
                (when (and
                        result-pos
                        (null target-obj)
                        (equal (jsown:val child "type") "METHOD"))
                  (let ((fq-name (get-fq-name-of-declaration root-ast
                                                             (jsown:val child "pos"))))
                    (when (and
                            (equal (car (last (split #\. fq-name))) name-with-params)
                            (equal fq-name (cdr (assoc :fq-name target-pos))))
                      (return-from
                        find-reference-pos
                        (list
                          (cons :path (get-original-path index-path))
                          (cons :top-offset result-pos))))))))

        (when (equal (cdar ast) "COMPILATION_UNIT")
          (loop for child in (jsown:val ast "children")
                do
                (when (and
                        (equal (jsown:val child "type") "IMPORT")
                        (equal (car (last (split #\. (jsown:val child "fqName")))) class-name))
                  (let ((split-import-names (split #\. (jsown:val child "fqName")))
                        fq-name
                        params)
                    (setf fq-name 
                          (concatenate
                            'string
                            (format nil "~{~a~^.~}"
                                    (append (subseq split-import-names
                                                    0 
                                                    (1- (length split-import-names)))
                                            (list class-name name-with-params)))))
                    (when (equal fq-name (cdr (assoc :fq-name target-pos)))
                      (return-from
                        find-reference-pos
                        (list
                          (cons :path (get-original-path index-path))
                          (cons :top-offset result-pos))))))))

        (when (jsown:keyp ast "parent")
          (enqueue q (jsown:val ast "parent")))))))

(defun get-fq-name-of-declaration (root-ast top-offset)
  (let ((stack (list root-ast))
        result)
    (loop
      with class-name
      do
      (let ((ast (pop stack)))
        (if (null ast) (return))

        (when (equal (cdar ast) "PACKAGE")
          (setf result (format nil "~{~a~^.~}" (remove nil (list result (jsown:val ast "packageName"))))))
        (when (or
                (equal (cdar ast) "CLASS")
                (equal (cdar ast) "INTERFACE"))
          (setf class-name (jsown:val ast "name"))
          (setf result (format nil "~{~a~^.~}" (remove nil (list result (jsown:val ast "name"))))))

        (when (and
                (jsown:keyp ast "name")
                (= (jsown:val ast "pos") top-offset))
          (setf result (format nil "~{~a~^.~}"
                               (remove nil (list result
                                                 (if (equal (jsown:val ast "name") "<init>")
                                                     class-name
                                                     (jsown:val ast "name"))))))
          (when (equal (cdar ast) "METHOD")
            (loop for child in (jsown:val ast "children")
                  do
                  (when (equal (jsown:val child "type") "VARIABLE")
                    (loop for child in (jsown:val child "children")
                          do
                          (when (jsown:keyp child "name")
                            (setf result (concatenate 'string result "-" (jsown:val child "name"))))))))
          (return-from get-fq-name-of-declaration result))

        (loop for child in (jsown:val ast "children")
              do (setf stack (append stack (list (cdr child)))))))))

