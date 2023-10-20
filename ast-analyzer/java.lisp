(defpackage #:inga/ast-analyzer/java
  (:use #:cl
        #:inga/ast-analyzer/base
        #:inga/utils)
  (:import-from #:quri)
  (:import-from #:inga/path
                #:merge-paths
                #:get-variable-names
                #:replace-variable-name)
  (:import-from #:inga/ast-analyzer/kotlin
                #:ast-analyzer-kotlin)
  (:import-from #:inga/plugin/jvm-dependency-loader
                #:load-signatures
                #:load-hierarchy)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path)
  (:import-from #:inga/plugin/spring-property-loader
                #:find-property)
  (:import-from #:inga/plugin/spring-helper
                #:convert-to-http-method)
  (:export #:ast-analyzer-java))
(in-package #:inga/ast-analyzer/java)

;; https://github.com/javaparser/javaparser/blob/d7a83612e1fa0c3c93ebac243a768339346382b5/javaparser-core/src/main/java/com/github/javaparser/JavaToken.java#L258
(defparameter *java-rbrace* 100) ;; }

(defclass ast-analyzer-java (ast-analyzer)
  ())

(defmethod start-ast-analyzer ((kind (eql :java)) exclude path)
  (setf *ast-analyzers*
        (acons :java
               (make-instance
                 'ast-analyzer-java
                 :process
                 (uiop:launch-program
                   (format nil "~{~a~^ ~}"
                           (list "java" "-cp"
                                 (format nil "~a/libs/javaparser.jar" (uiop:getenv "INGA_HOME"))
                                 "--add-opens" "jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED"
                                 "--add-opens" "jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED"
                                 "--add-opens" "jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED"
                                 "inga.Main"))
                   :input :stream :output :stream)
                 :path path) 
               *ast-analyzers*))
  (cdr (assoc :java *ast-analyzers*)))

(defmethod stop-ast-analyzer ((ast-analyzer ast-analyzer-java))
  (uiop:close-streams (ast-analyzer-process ast-analyzer)))

(defmethod find-definitions-generic ((ast-analyzer ast-analyzer-java) range)
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
             (return-from find-definitions-generic)))
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
                    (or
                      (ast-value
                        (first (ast-get request-mapping '("STRING_LITERAL")))
                        "name")
                      (ast-value
                        (first (ast-get request-mapping '("ASSIGNMENT" "STRING_LITERAL")))
                        "name")))))))
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
                     (cons :type (get-scope ast))
                     (cons :path src-path)
                     (if (equal (jsown:val ast "name") "<init>")
                         (cons :name class-name)
                         (cons :name (jsown:val ast "name")))
                     (cons :fq-name (get-fq-name-of-declaration
                                      root-ast
                                      (jsown:val ast "pos")
                                      index-path))
                     (cons :top-offset (jsown:val ast "pos")))))
          (when is-entrypoint-file
            (let ((mapping (first (ast-find-names
                                        (ast-get ast '("MODIFIERS" "ANNOTATION"))
                                        '("GetMapping" "PostMapping" "PutMapping" "DeleteMapping"))))
                  (port (find-property "server.port" (get-original-path index-path))))
              (when (and mapping port)
                (setf pos
                      (list
                        (cons :type :rest-server)
                        (cons :host port)
                        (cons :name (convert-to-http-method (ast-value mapping "name")))
                        (let ((path (merge-paths 
                                      entrypoint-name
                                      (ast-value
                                        (first (ast-get mapping '("STRING_LITERAL")))
                                        "name"))))
                          (loop for vn in (get-variable-names path)
                                do
                                (let ((v (find-variable vn ast index-path)))
                                  (let ((path-variable (first (ast-find-name
                                                                (ast-get v '("MODIFIERS"
                                                                             "ANNOTATION"))
                                                                "PathVariable"))))
                                    (when (ast-find-name
                                            (ast-get path-variable '("STRING_LITERAL"))
                                            vn)
                                      (setf path (replace-variable-name
                                                   path vn
                                                   (convert-to-json-type (find-variable-name vn v index-path))))))))
                          (cons :path path))
                        (cons :file-pos pos))))))
          (when (assoc :origin range)
            (push (cons :origin (cdr (assoc :origin range))) pos))
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

(defun get-scope (ast)
  (let ((modifiers (split-trim-comma (ast-value (first (ast-get ast '("MODIFIERS"))) "name"))))
    (cond
      ((find "PUBLIC" modifiers :test #'equal) :module-public)
      ((find "PROTECTED" modifiers :test #'equal) :module-protected)
      ((find "PRIVATE" modifiers :test #'equal) :module-private)
      (t :module-default))))

(defmethod find-reference ((ast-analyzer ast-analyzer-java) target-pos ast index-path)
  (let ((fq-name (find-fq-name-for-reference ast index-path)))
    (unless fq-name (return-from find-reference))

    (alexandria:switch ((cdr (assoc :type target-pos)))
      (:rest-server
        (let ((rest-client (find-rest-client fq-name ast index-path)))
          (when (and
                  (equal (cdr (assoc :host rest-client)) (cdr (assoc :host target-pos)))
                  (equal (cdr (assoc :path rest-client)) (cdr (assoc :path target-pos)))
                  (equal (cdr (assoc :name rest-client)) (cdr (assoc :name target-pos))))
            `((:path . ,(get-original-path index-path))
              (:top-offset . ,(ast-value ast "startPos"))))))
      (t
        (when (equal fq-name (cdr (assoc :fq-name target-pos)))
          `((:path . ,(get-original-path index-path))
            (:top-offset . ,(ast-value ast "startPos"))))))))

(defmethod find-signatures-generic ((ast-analyzer ast-analyzer-java) fq-class-name root-ast)
  (loop
    with stack = (list root-ast)
    with ast
    with target-package-name = (format nil "~{~a~^.~}" (butlast (split #\. fq-class-name)))
    with target-class-name = (first (last (split #\. fq-class-name)))
    with package-name
    with class-name
    with results
    do
    (setf ast (pop stack))
    (if (null ast) (return results))

    (when (equal (ast-value ast "type") "PACKAGE")
      (unless (equal (ast-value ast "packageName") target-package-name)
        (return-from find-signatures-generic))
      (setf package-name (ast-value ast "packageName")))
    (when (or
            (equal (ast-value ast "type") "CLASS")
            (equal (ast-value ast "type") "RECORD")
            (equal (ast-value ast "type") "INTERFACE"))
      (unless (equal (ast-value ast "name") target-class-name)
        (return-from find-signatures-generic))
      (setf class-name (ast-value ast "name")))
    (when (equal (ast-value ast "type") "VARIABLE")
      (setf results (append results (list
                                      `(:obj
                                         ("kind" . "variable")
                                         ("name" . ,(ast-value ast "name"))
                                         ("type" . ,(find-fq-class-name
                                                      (ast-value (first (ast-get ast '("IDENTIFIER"))) "name")
                                                      ast)))))))

    (loop for child in (jsown:val ast "children")
          do (setf stack (append stack (list child))))))

(defmethod find-class-hierarchy-generic ((ast-analyzer ast-analyzer-java) fq-class-name root-ast index-path)
  (loop
    with stack = (list root-ast)
    with ast
    with target-package-name = (format nil "~{~a~^.~}" (butlast (split #\. fq-class-name)))
    with target-class-name = (first (last (split #\. fq-class-name)))
    initially
    (let ((hierarchy (load-hierarchy fq-class-name (get-original-path index-path))))
      (when hierarchy
        (return-from find-class-hierarchy-generic hierarchy)))
    do
    (setf ast (pop stack))
    (if (null ast) (return))

    (when (equal (ast-value ast "type") "PACKAGE")
      (unless (equal (ast-value ast "packageName") target-package-name)
        (return-from find-class-hierarchy-generic)))
    (when (equal (ast-value ast "type") "CLASS")
      (unless (equal (ast-value ast "name") target-class-name)
        (return-from find-class-hierarchy-generic))
      (return-from find-class-hierarchy-generic
        (let ((parent-class-name (ast-value (first (ast-get ast '("IDENTIFIER"))) "name")))
          (if parent-class-name
              (let ((parent-fq-class-name (find-fq-class-name parent-class-name ast)))
                (when parent-fq-class-name
                  (append (find-class-hierarchy parent-fq-class-name)
                          (list parent-fq-class-name)
                          (list fq-class-name))))
              '("java.lang.Object")))))
    (loop for child in (jsown:val ast "children")
          do (setf stack (append stack (list child))))))

(defmethod find-package-index-key-generic ((ast-analyzer ast-analyzer-java) ast)
  (loop
    with stack = (list ast)
    do
    (setf ast (pop stack))
    (if (null ast) (return))

    (when (equal (ast-value ast "type") "PACKAGE")
      (return-from find-package-index-key-generic (intern (ast-value ast "packageName"))))

    (loop for child in (jsown:val ast "children")
          do (setf stack (append stack (list child))))))

(defmethod find-project-index-key-generic ((ast-analyzer ast-analyzer-java) path)
  (let ((base-path (find-base-path path)))
    (when base-path (intern (namestring base-path)))))

(defun find-fq-name-for-reference (ast index-path)
  (alexandria:switch ((ast-value ast "type") :test #'equal)
    ("NEW_CLASS"
     (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
             (find-fq-class-name (ast-value ast "name") ast)
             (ast-value ast "name")
             (find-method-args (ast-get ast '("*")) index-path)))
    ("METHOD_INVOCATION"
     (if (ast-get ast '("MEMBER_SELECT"))
         (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
                 ;; if method chain
                 (if (ast-get ast '("MEMBER_SELECT" "METHOD_INVOCATION"))
                     (let ((fq-name (find-fq-name-for-reference
                                      (first (ast-get ast '("MEMBER_SELECT" "METHOD_INVOCATION")))
                                      index-path)))
                       (when fq-name
                         (let ((method (find-signature
                                         fq-name
                                         #'(lambda (fq-class-name) (load-signatures fq-class-name (get-original-path index-path))))))
                           (when method
                             (jsown:val (jsown:val method "returnType") "name")))))
                     (if (ast-get ast '("MEMBER_SELECT" "IDENTIFIER"))
                         (or (find-variable-name
                               (ast-value (first (ast-get ast '("MEMBER_SELECT" "IDENTIFIER"))) "name")
                               ast
                               index-path)
                             (find-fq-class-name
                               ;; IDENTIFIER is class name
                               (ast-value (first (ast-get ast '("MEMBER_SELECT" "IDENTIFIER"))) "name")
                               ast))
                         (find-fq-class-name
                           (ast-value (first (ast-get ast '("MEMBER_SELECT" "NEW_CLASS"))) "name")
                           ast)))
                 (ast-value (first (ast-get ast '("*"))) "name")
                 (find-method-args (nthcdr 1 (ast-get ast '("*"))) index-path))
         (format nil "~a~:[~;-~]~:*~{~a~^-~}"
                 (find-fq-name-for-definition
                   (ast-value (first (ast-get ast '("IDENTIFIER"))) "name")
                   ast)
                 (find-method-args (nthcdr 1 (ast-get ast '("*"))) index-path))))))

(defun find-fq-class-name (class-name ast)
  (unless class-name
    (return-from find-fq-class-name))

  (cond
    ((find class-name '("INT") :test 'equal)
     class-name)
    ((find class-name '("Long" "String") :test 'equal)
     (concatenate 'string "java.lang." class-name))
    (t
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
           (setf fq-names
                 (if import
                     (list (ast-value import "fqName"))
                     (list
                       (ast-value (first (ast-get ast '("PACKAGE"))) "packageName")
                       class-name)))))

       (when (jsown:keyp ast "parent")
         (enqueue q (jsown:val ast "parent")))))))

(defun find-variable-name (object-name ast index-path)
  (let ((variable (find-variable object-name ast index-path)))
    (cond
      ((ast-get variable '("IDENTIFIER"))
       (find-fq-class-name (ast-value (first (ast-get variable '("IDENTIFIER"))) "name") variable))
      ((ast-get variable '("METHOD_INVOCATION"))
       (let ((fq-name (find-fq-name-for-reference (first (ast-get variable '("METHOD_INVOCATION"))) index-path)))
         (let ((method (find-signature fq-name
                                       #'(lambda (fq-class-name) (load-signatures fq-class-name (get-original-path index-path))))))
           (when method
             (jsown:val (jsown:val method "returnType") "name"))))))))

(defun find-variable (object-name ast index-path)
  (unless object-name
    (return-from find-variable))

  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (when (null ast) (return))

    (let ((variable (first (if (equal (ast-value ast "type") "VARIABLE")
                               (ast-find-name (list ast) object-name)
                               (ast-find-name (ast-get ast '("VARIABLE")) object-name)))))
      (when variable
        (return variable)))

    (enqueue q (ast-value ast "parent"))))

(defun find-method-args (ast index-path)
  (loop for arg in ast
        with results
        do
        (setf results
              (append results
                      (list
                        (if (uiop:string-suffix-p (ast-value arg "type") "_LITERAL")
                            (if (equal (ast-value arg "type") "STRING_LITERAL")
                                "java.lang.String"
                                (ppcre:regex-replace-all "_LITERAL" (ast-value arg "type") ""))
                            (alexandria:switch ((jsown:val arg "type") :test #'equal)
                              ("MEMBER_SELECT"
                               (if (ast-find-name (list arg) "class")
                                   "Class"
                                   (ast-value (first (ast-get arg '("IDENTIFIER"))) "name")))
                              ("NEW_CLASS"
                               (find-fq-class-name (ast-value arg "name") arg))
                              ("IDENTIFIER"
                               (find-variable-name (ast-value arg "name") arg index-path))
                              ("METHOD_INVOCATION"
                               (let ((fq-name (find-fq-name-for-reference arg index-path)))
                                 (let ((method (find-signature fq-name
                                                               #'(lambda (fq-class-name)
                                                                   (load-signatures fq-class-name (get-original-path index-path))))))
                                   (if method
                                       (jsown:val (jsown:val method "returnType") "name")
                                       (ast-value
                                         (find-signature fq-name #'find-signatures)
                                         "type")))))
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

(defun find-rest-client (fq-name ast index-path)
  ;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
  (cond
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.exchange-java.lang.String-HttpMethod-NULL-Class")
     `((:host . ,(find-api-host 0 ast))
       (:name . ,(find-api-method-from-http-method (nth 2 (ast-get ast '("*")))))
       (:path . ,(find-api-path 0 ast))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-Class")
     `((:host . ,(find-api-host 0 ast))
       (:name . "GET")
       (:path . ,(find-api-path 0 ast))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.getForObject-java.net.URI-Class")
     (let ((server (find-server-from-uri 0 ast index-path)))
       `((:host . ,(cdr (assoc :host server)))
         (:name . "GET")
         (:path . ,(cdr (assoc :path server))))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.postForObject-java.lang.String-java.lang.Object-Class")
     `((:host . ,(find-api-host 0 ast))
       (:name . "POST")
       (:path . ,(find-api-path 0 ast))))))

(defun find-api-method-from-http-method (http-method)
  (ast-value http-method "name"))

(defun find-api-host (arg-i ast)
  (let ((url (get-parameter arg-i ast)))
    (when (equal (ast-value url "type") "STRING_LITERAL")
      (format nil "~a" (quri:uri-port (quri:uri (ast-value url "name")))))))

(defun find-api-path (arg-i ast)
  (let ((url (get-parameter arg-i ast)))
    (when (equal (ast-value url "type") "STRING_LITERAL")
      (quri:uri-path (quri:uri (ast-value url "name"))))))

(defun get-parameter (idx ast)
  (nth (1+ idx) (ast-get ast '("*"))))

(defun find-server-from-uri (arg-i ast index-path)
  (let ((param-uri (nth (1+ arg-i) (ast-get ast '("*")))))
    (let ((variable-uri (find-variable (ast-value param-uri "name") param-uri index-path))
          host
          path)
      (labels ((find-uri-components-builder (ast index-path)
                 (let ((fq-name (find-fq-name-for-reference ast index-path)))
                   (unless fq-name (return-from find-uri-components-builder))
                   (cond
                     ((equal
                        fq-name
                        "org.springframework.web.util.UriComponentsBuilder.path-java.lang.String")
                      (setf path (format nil "/{~a}"
                                         (convert-to-json-type
                                           (find-variable-name
                                             (ast-value (get-parameter 0 ast) "name")
                                             ast
                                             index-path)))))
                     ((equal
                        fq-name
                        "org.springframework.web.util.UriComponentsBuilder.fromUriString-java.lang.String")
                      (let ((v (find-variable
                                 (ast-value (get-parameter 0 ast) "name")
                                 ast
                                 index-path)))
                        (let ((value (first (ast-find-name
                                              (ast-get v '("MODIFIERS"
                                                           "ANNOTATION"))
                                              "Value"))))
                          (setf host
                                (write-to-string
                                  (quri:uri-port
                                    (quri:uri 
                                      (find-property
                                        (first (get-variable-names
                                                 (ast-value (first (ast-get value '("STRING_LITERAL"))) "name")))
                                        (get-original-path index-path))))))))))
                   (find-uri-components-builder
                     (first (ast-get ast '("MEMBER_SELECT" "METHOD_INVOCATION")))
                     index-path))))
        (find-uri-components-builder
          (first (ast-get variable-uri '("METHOD_INVOCATION")))
          index-path))
      `((:host . ,host)
        (:path . ,path)))))

(defun convert-to-json-type (type)
  (alexandria:switch (type :test #'equal)
    ("java.lang.String" "string")
    ("INT" "number")))

(defun get-fq-name-of-declaration (root-ast top-offset index-path)
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
                          (when (and (ast-value child "name")
                                     (not (equal (ast-value child "name") "")))
                            (setf result (concatenate
                                           'string
                                           result
                                           "-"
                                           (find-fq-class-name (ast-value child "name") ast))))))))
          (return-from get-fq-name-of-declaration result))

        (loop for child in (jsown:val ast "children")
              do (setf stack (append stack (list child))))))))

