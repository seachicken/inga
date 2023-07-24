(defpackage #:inga/ast-analyzer/java
  (:use #:cl
        #:inga/ast-analyzer/base
        #:inga/utils)
  (:import-from #:quri)
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
      with is-entrypoint-file
      with entrypoint-name
      do
      (setf ast (dequeue q))
      (if (null ast) (return))

      (when (equal (ast-value ast "type") "CLASS")
        (setf class-name (jsown:val ast "name"))
        (let ((annotations (ast-get ast '("MODIFIERS" "ANNOTATION"))))
          (when (ast-find-name annotations "RestController")
            (setf is-entrypoint-file t))
          (let ((request-mapping (first (ast-find-name annotations "RequestMapping"))))
            (when request-mapping
              (setf entrypoint-name 
                    (jsown:val
                      (first (ast-get request-mapping '("ASSIGNMENT" "STRING_LITERAL")))
                      "name"))))))
      (when (and
              (or
                ;; field reference
                (and
                  (jsown:keyp ast "parent")
                  (equal (ast-value (jsown:val ast "parent") "type") "CLASS") 
                  (equal (ast-value ast "type") "VARIABLE"))
                  (equal (ast-value ast "type") "METHOD"))
              (contains-offset (jsown:val ast "startPos") (jsown:val ast "endPos")
                               start-offset end-offset)
              (jsown:keyp ast "name"))
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
          (when is-entrypoint-file
            (let ((annotations (ast-get ast '("MODIFIERS" "ANNOTATION"))))
              ;; TODO: imprements other methods
              (when (ast-find-name annotations "GetMapping")
                (setf pos
                      (list
                        (cons :type :rest-server)
                        (cons :path entrypoint-name)
                        (cons :name "GET")
                        (cons :file-pos pos))))))
          (setf results (append results (list pos)))))

      (when (and
              (string= (ast-value ast "type")
                       "com.github.javaparser.ast.body.ClassOrInterfaceDeclaration")
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
                (string= (ast-value ast "type")
                         "com.github.javaparser.ast.body.FieldDeclaration")
                (string= (ast-value ast "type")
                         "com.github.javaparser.ast.body.ConstructorDeclaration")
                (string= (ast-value ast "type")
                         "com.github.javaparser.ast.body.MethodDeclaration"))
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
            (setf (jsown:val child "parent") ast)
            (enqueue q child)))
    results))

(defmethod find-entrypoint ((ast-analyzer ast-analyzer-java) pos))

(defmethod find-reference ((ast-analyzer ast-analyzer-java) target-pos ast index-path)
  (let ((fq-name (find-fq-name-for-reference ast-analyzer ast)))
    (unless fq-name (return-from find-reference))

    (alexandria:switch ((cdr (assoc :type target-pos)))
      (:rest-server
        (let ((rest-client (find-rest-client fq-name ast)))
          (when (and
                  (equal (cdr (assoc :path rest-client)) (cdr (assoc :path target-pos)))
                  (equal (cdr (assoc :name rest-client)) (cdr (assoc :name target-pos))))
            (list
              (cons :path (get-original-path index-path))
              (cons :top-offset (ast-value ast "startPos"))))))
      (t
        (when (equal fq-name (cdr (assoc :fq-name target-pos)))
          (list
            (cons :path (get-original-path index-path))
            (cons :top-offset (ast-value ast "startPos"))))))))

(defmethod find-fq-name-for-reference ((ast-analyzer ast-analyzer-java) ast)
  (alexandria:switch ((ast-value ast "type") :test #'equal)
    ("NEW_CLASS"
     (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
             (find-fq-class-name (ast-value ast "name") ast)
             (ast-value ast "name")
             (find-method-args (ast-get ast '("*")))))
    ("METHOD_INVOCATION"
     (if (ast-get ast '("MEMBER_SELECT"))
         (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
                 (find-fq-class-name
                   (if (ast-get ast '("MEMBER_SELECT" "NEW_CLASS"))
                       (ast-value (first (ast-get ast '("MEMBER_SELECT" "NEW_CLASS"))) "name")
                       (find-variable-name
                         (ast-value (first (ast-get ast '("MEMBER_SELECT" "IDENTIFIER"))) "name")
                         ast))
                   ast)
                 (ast-value (first (ast-get ast '("*"))) "name")
                 (find-method-args (nthcdr 1 (ast-get ast '("*")))))
         (format nil "~a~:[~;-~]~:*~{~a~^-~}"
                 (find-fq-name-for-definition
                   (ast-value (first (ast-get ast '("IDENTIFIER"))) "name")
                   ast)
                 (find-method-args (nthcdr 1 (ast-get ast '("*")))))))))

(defun find-fq-class-name (class-name ast)
  (loop
    with q = (make-queue)
    with fq-names
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (when (null ast) (return (format nil "~{~a~^.~}" fq-names)))

    (when (equal (ast-value ast "type") "COMPILATION_UNIT")
      (let ((import (first (ast-find-suffix
                             (ast-get ast '("IMPORT"))
                             (concatenate 'string "." class-name)
                             :key-name "fqName"))))
        (when import
          (setf fq-names (append fq-names (list (ast-value import "fqName")))))))

    (when (jsown:keyp ast "parent")
      (enqueue q (jsown:val ast "parent")))))

(defun find-variable-name (object-name ast)
  (unless object-name
    (return-from find-variable-name))

  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (when (null ast) (return))

    (let ((variable (first (ast-find-name (ast-get ast '("VARIABLE")) object-name))))
      (when variable
        (return (ast-value (first (ast-get variable '("IDENTIFIER"))) "name"))))

    (enqueue q (ast-value ast "parent"))))

(defun find-method-args (ast)
  (loop for arg in ast
        with results
        do
        (setf results
              (append results
                      (list
                        (if (uiop:string-suffix-p (ast-value arg "type") "_LITERAL")
                            (if (equal (ast-value arg "type") "STRING_LITERAL")
                                "String"
                                (ppcre:regex-replace-all "_LITERAL" (ast-value arg "type") ""))
                            (alexandria:switch ((jsown:val arg "type") :test #'equal)
                              ("MEMBER_SELECT"
                               (if (ast-find-name (list arg) "class")
                                   "Class"
                                   (ast-value (first (ast-get arg '("IDENTIFIER"))) "name")))
                              ("NEW_CLASS"
                               (ast-value arg "name"))
                              ("IDENTIFIER"
                               (find-variable-name (ast-value arg "name") arg))
                              (t
                               "?"))))))
        finally (return results)))

(defun find-fq-name-for-definition (method-name ast)
  (loop
    with q = (make-queue)
    with fq-names
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (when (null ast) (return (format nil "~{~a~^.~}" fq-names)))

    (when (ast-find-name (ast-get ast '("METHOD")) method-name)
      (setf fq-names (append fq-names (list method-name))))
    (when (and
            fq-names
            (equal (ast-value ast "type") "CLASS"))
      (setf fq-names (append (list (ast-value ast "name")) fq-names)))
    (when (and
            fq-names
            (equal (ast-value ast "type") "COMPILATION_UNIT"))
      (setf fq-names (append 
                       (list (ast-value (first (ast-get ast '("PACKAGE"))) "packageName"))
                       fq-names)))

    (when (jsown:keyp ast "parent")
      (enqueue q (jsown:val ast "parent")))))

(defun find-rest-client (fq-name ast)
  (alexandria:switch (fq-name :test #'equal)
    ;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
    ("org.springframework.web.client.RestTemplate.exchange-String-HttpMethod-NULL-Class"
     `((:name . ,(find-api-method-from-http-method (nth 2 (ast-get ast '("*")))))
       (:path . ,(find-api-path 0 ast))))
    ("org.springframework.web.client.RestTemplate.getForObject-String-Class"
     `((:name . "GET")
       (:path . ,(find-api-path 0 ast))))))

(defun find-api-method-from-http-method (http-method)
  (ast-value http-method "name"))

(defun find-api-path (arg-i ast)
  (let ((url (nth (1+ arg-i) (ast-get ast '("*")))))
    (when (equal (ast-value url "type") "STRING_LITERAL")
      (quri:uri-path (quri:uri (ast-value url "name"))))))

(defun get-fq-name-of-declaration (root-ast top-offset)
  (let ((stack (list root-ast))
        result)
    (loop
      with class-name
      do
      (let ((ast (pop stack)))
        (if (null ast) (return))

        (when (equal (ast-value ast "type") "PACKAGE")
          (setf result (format nil "~{~a~^.~}" (remove nil (list result (jsown:val ast "packageName"))))))
        (when (or
                (equal (ast-value ast "type") "CLASS")
                (equal (ast-value ast "type") "INTERFACE"))
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
          (when (equal (ast-value ast "type") "METHOD")
            (loop for child in (jsown:val ast "children")
                  do
                  (when (equal (jsown:val child "type") "VARIABLE")
                    (loop for child in (jsown:val child "children")
                          do
                          (when (jsown:keyp child "name")
                            (setf result (concatenate 'string result "-" (jsown:val child "name"))))))))
          (return-from get-fq-name-of-declaration result))

        (loop for child in (jsown:val ast "children")
              do (setf stack (append stack (list child))))))))

