(defpackage #:inga/ast-analyzer/java
  (:use #:cl
        #:inga/ast-analyzer/base
        #:inga/utils)
  (:import-from #:quri)
  (:import-from #:inga/path
                #:merge-paths
                #:get-variable-names
                #:replace-variable-name)
  (:import-from #:inga/ast-index
                #:ast-index-paths
                #:ast-index-root-path
                #:clean-indexes
                #:create-indexes
                #:get-ast)
  (:import-from #:inga/ast-analyzer/kotlin
                #:ast-analyzer-kotlin)
  (:import-from #:inga/file
                #:get-file-type)
  (:import-from #:inga/plugin/jvm-dependency-loader
                #:load-signatures
                #:load-hierarchy)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path
                #:is-primitive-type)
  (:import-from #:inga/plugin/spring-property-loader
                #:find-property)
  (:import-from #:inga/plugin/spring-helper
                #:convert-to-http-method)
  (:export #:ast-analyzer-java))
(in-package #:inga/ast-analyzer/java)

(defvar *package-index-groups* nil)
(defvar *project-index-groups* nil)

(defparameter *include-java* '("*.java"))

(defclass ast-analyzer-java (ast-analyzer)
  ())

(defmethod start-ast-analyzer ((kind (eql :java)) include exclude path index)
  (setf *ast-analyzers*
        (acons :java
               (make-instance 'ast-analyzer-java
                              :path path
                              :index index)
               *ast-analyzers*))
  (create-indexes index include *include-java* exclude)
  (create-index-groups index)
  (cdr (assoc :java *ast-analyzers*)))

(defmethod stop-ast-analyzer ((ast-analyzer ast-analyzer-java))
  (clean-index-groups (ast-analyzer-index ast-analyzer)) 
  (clean-indexes (ast-analyzer-index ast-analyzer))
  (setf *ast-analyzers* nil))

(defun create-index-groups (index)
  (loop for path in (remove-if-not (lambda (p) (eq (get-file-type p) :java))
                                   (ast-index-paths index))
        do
        (let ((index-key (find-package-index-key (get-ast index path))))
          (setf *package-index-groups*
                (if (assoc index-key *package-index-groups*)
                    (acons index-key
                           (append (list path) (cdr (assoc index-key *package-index-groups*)))
                           *package-index-groups*)
                    (acons index-key (list path) *package-index-groups*))))
        (let ((index-key (find-project-index-key
                           (merge-pathnames path (ast-index-root-path index)))))
          (setf *project-index-groups*
                (if (assoc index-key *project-index-groups*)
                    (acons index-key
                           (append (list path) (cdr (assoc index-key *project-index-groups*)))
                           *project-index-groups*)
                    (acons index-key (list path) *project-index-groups*))))))

(defun clean-index-groups (index)
  (setf *package-index-groups* nil)
  (setf *project-index-groups* nil))

(defmethod get-scoped-index-paths-generic ((ast-analyzer ast-analyzer-java) pos)
  (cond
    ((eq (cdr (assoc :type pos)) :module-private)
     (list (cdr (assoc :path pos))))
    ((eq (cdr (assoc :type pos)) :module-default)
     (cdr (assoc
            (find-package-index-key (get-ast (ast-analyzer-index ast-analyzer) (cdr (assoc :path pos))))
            *package-index-groups*)))
    ((eq (cdr (assoc :type pos)) :module-public)
     (cdr (assoc
            (find-project-index-key (merge-pathnames (cdr (assoc :path pos)) (ast-analyzer-path ast-analyzer)))
            *project-index-groups*)))
    (t
     (ast-index-paths (ast-analyzer-index ast-analyzer)))))

(defun find-package-index-key (ast)
  (loop
    with stack = (list ast)
    do
    (setf ast (pop stack))
    (if (null ast) (return))

    (when (equal (ast-value ast "type") "PACKAGE")
      (return-from find-package-index-key (intern (ast-value ast "packageName"))))

    (loop for child in (jsown:val ast "children")
          do (setf stack (append stack (list child))))))

(defun find-project-index-key (path)
  (let ((base-path (find-base-path path)))
    (when base-path (intern (namestring base-path)))))

(defmethod find-definitions-generic ((ast-analyzer ast-analyzer-java) range)
  (let ((q (make-queue))
        (src-path (cdr (assoc :path range)))
        (path (cdr (assoc :path range)))
        (start-offset (cdr (assoc :start-offset range)))
        (end-offset (cdr (assoc :end-offset range)))
        ast
        root-ast
        results)
    (setf ast (get-ast (ast-analyzer-index ast-analyzer) path))
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
                                      (jsown:val ast "pos")))
                     (cons :top-offset (jsown:val ast "pos")))))
          (when is-entrypoint-file
            (let ((mapping (first (ast-find-names
                                        (ast-get ast '("MODIFIERS" "ANNOTATION"))
                                        '("GetMapping" "PostMapping" "PutMapping" "DeleteMapping"))))
                  (port (find-property "server.port" path)))
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
                                (let ((v (find-variable vn ast)))
                                  (let ((path-variable (first (ast-find-name
                                                                (ast-get v '("MODIFIERS"
                                                                             "ANNOTATION"))
                                                                "PathVariable"))))
                                    (when (ast-find-name
                                            (ast-get path-variable '("STRING_LITERAL"))
                                            vn)
                                      (setf path (replace-variable-name
                                                   path vn
                                                   (convert-to-json-type
                                                     (find-variable-fq-class-name
                                                       vn v path
                                                       (ast-analyzer-index ast-analyzer)))))))))
                          (cons :path path))
                        (cons :file-pos pos))))))
          (when (assoc :origin range)
            (push (cons :origin (cdr (assoc :origin range))) pos))
          (setf results (append results (list pos)))))

      (loop for child in (jsown:val ast "children") do (enqueue q child)))
    results))

(defun get-scope (ast)
  (let ((modifiers (split-trim-comma (ast-value (first (ast-get ast '("MODIFIERS"))) "name"))))
    (cond
      ((find "PUBLIC" modifiers :test #'equal) :module-public)
      ((find "PROTECTED" modifiers :test #'equal) :module-protected)
      ((find "PRIVATE" modifiers :test #'equal) :module-private)
      (t :module-default))))

(defmethod find-reference ((ast-analyzer ast-analyzer-java) target-pos ast path)
  (let ((fq-name (find-fq-name-for-reference ast path (ast-analyzer-index ast-analyzer))))
    (unless fq-name (return-from find-reference))

    (alexandria:switch ((cdr (assoc :type target-pos)))
      (:rest-server
        (let ((rest-client (find-rest-client fq-name ast path (ast-analyzer-index ast-analyzer))))
          (when (and
                  (equal (cdr (assoc :host rest-client)) (cdr (assoc :host target-pos)))
                  (equal (cdr (assoc :path rest-client)) (cdr (assoc :path target-pos)))
                  (equal (cdr (assoc :name rest-client)) (cdr (assoc :name target-pos))))
            `((:path . ,path)
              (:top-offset . ,(ast-value ast "startPos"))))))
      (t
        (when (matches-signature fq-name (cdr (assoc :fq-name target-pos))
                                 (ast-analyzer-index ast-analyzer))
          `((:path . ,path)
            (:top-offset . ,(ast-value ast "startPos"))))))))

(defmethod find-class-hierarchy-generic ((ast-analyzer ast-analyzer-java)
                                         fq-class-name root-ast path index)
  (loop
    with stack = (list root-ast)
    with ast
    with target-package-name = (format nil "~{~a~^.~}" (butlast (split #\. fq-class-name)))
    with target-class-name = (first (last (split #\. fq-class-name)))
    initially
    (let ((hierarchy (load-hierarchy fq-class-name path)))
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
                  (append (find-class-hierarchy parent-fq-class-name index)
                          (list parent-fq-class-name)
                          (list fq-class-name))))
              '("java.lang.Object")))))
    (loop for child in (jsown:val ast "children")
          do (setf stack (append stack (list child))))))

(defun find-fq-name-for-reference (ast path index)
  (alexandria:switch ((ast-value ast "type") :test #'equal)
    ("NEW_CLASS"
     (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
             (find-fq-class-name (ast-value ast "name") ast)
             (ast-value ast "name")
             (find-method-args (ast-get ast '("*")) path index)))
    ("METHOD_INVOCATION"
     (if (ast-get ast '("MEMBER_SELECT"))
         (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
                 ;; if method chain
                 (cond
                   ((ast-get ast '("MEMBER_SELECT" "METHOD_INVOCATION"))
                    (let ((fq-name (find-fq-name-for-reference
                                     (first (ast-get ast '("MEMBER_SELECT" "METHOD_INVOCATION")))
                                     path index)))
                      (when fq-name
                        (let ((method (find-signature
                                        fq-name
                                        #'(lambda (fq-class-name)
                                            (load-signatures fq-class-name path))
                                        index)))
                          (when method
                            (jsown:val (jsown:val method "returnType") "name"))))))
                   ((ast-get ast '("MEMBER_SELECT" "IDENTIFIER"))
                    (or (find-variable-fq-class-name
                          (ast-value (first (ast-get ast '("MEMBER_SELECT" "IDENTIFIER"))) "name")
                          ast path index)
                        (find-fq-class-name
                          ;; IDENTIFIER is class name
                          (ast-value (first (ast-get ast '("MEMBER_SELECT" "IDENTIFIER"))) "name")
                          ast)))
                   ((ast-get ast '("MEMBER_SELECT" "NEW_CLASS"))
                    (find-fq-class-name
                      (ast-value (first (ast-get ast '("MEMBER_SELECT" "NEW_CLASS"))) "name")
                      ast))
                   ((ast-get ast '("MEMBER_SELECT" "MEMBER_SELECT"))
                    (find-fq-class-name
                      (ast-value (first (ast-get ast '("MEMBER_SELECT" "MEMBER_SELECT"))) "name")
                      ast)))
                 (ast-value (first (ast-get ast '("*"))) "name")
                 (find-method-args (nthcdr 1 (ast-get ast '("*"))) path index))
         (format nil "~a~:[~;-~]~:*~{~a~^-~}"
                 (find-fq-name-for-definition
                   (ast-value (first (ast-get ast '("IDENTIFIER"))) "name")
                   ast)
                 (find-method-args (nthcdr 1 (ast-get ast '("*"))) path index))))))

(defun find-fq-class-name (class-name ast)
  (unless class-name
    (return-from find-fq-class-name))

  (cond
    ((is-primitive-type class-name)
     class-name)
    ((find class-name '("Long" "String") :test 'equal)
     (concatenate 'string "java.lang." class-name))
    ((equal class-name "class")
     "java.lang.Class")
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
                     (append
                       (list (ast-value (first (ast-get ast '("PACKAGE"))) "packageName"))
                       fq-names)))))

       (when (equal (ast-value ast "type") "CLASS")
         (push class-name fq-names)
         ;; for inner class
         (when (or (ast-find-name (ast-get ast '("CLASS")) class-name)
                   (ast-find-name (ast-get ast '("ENUM")) class-name))
           (push (ast-value ast "name") fq-names)))

       (when (jsown:keyp ast "parent")
         (enqueue q (jsown:val ast "parent")))))))

(defun find-variable-fq-class-name (object-name ast path index)
  (let ((variable (find-variable object-name ast)))
    (cond
      ((ast-get variable '("IDENTIFIER"))
       (find-fq-class-name (ast-value (first (ast-get variable '("IDENTIFIER"))) "name") variable))
      ((ast-get variable '("PARAMETERIZED_TYPE"))
       (find-fq-class-name (ast-value (first (ast-get variable '("PARAMETERIZED_TYPE"))) "name") variable))
      ((ast-get variable '("METHOD_INVOCATION"))
       (let ((fq-name (find-fq-name-for-reference
                        (first (ast-get variable '("METHOD_INVOCATION"))) path index)))
         (let ((method (find-signature
                         fq-name
                         #'(lambda (fq-class-name)
                             (load-signatures fq-class-name path))
                         index)))
           (when method
             (jsown:val (jsown:val method "returnType") "name"))))))))

(defun find-variable (object-name ast)
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

(defun find-method-args (ast path index)
  (mapcar (lambda (arg)
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
                   (find-variable-fq-class-name (ast-value arg "name") arg path index))
                  ("METHOD_INVOCATION"
                   (let ((fq-name (find-fq-name-for-reference arg path index)))
                     (let ((method (find-signature
                                     fq-name
                                     #'(lambda (fq-class-name)
                                         (load-signatures fq-class-name path))
                                     index)))
                       (when method (jsown:val (jsown:val method "returnType") "name"))))))))
          ast))

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

(defun find-rest-client (fq-name ast path index)
  ;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
  (cond
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.exchange-java.lang.String-HttpMethod-NULL-Class"
       index)
     `((:host . ,(find-api-host 0 ast))
       (:name . ,(find-api-method-from-http-method (nth 2 (ast-get ast '("*")))))
       (:path . ,(find-api-path 0 ast))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-Class"
       index)
     `((:host . ,(find-api-host 0 ast))
       (:name . "GET")
       (:path . ,(find-api-path 0 ast))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.getForObject-java.net.URI-Class"
       index)
     (let ((server (find-server-from-uri 0 ast path index)))
       `((:host . ,(cdr (assoc :host server)))
         (:name . "GET")
         (:path . ,(cdr (assoc :path server))))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.postForObject-java.lang.String-java.lang.Object-Class"
       index)
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

(defun find-server-from-uri (arg-i ast path index)
  (let ((param-uri (nth (1+ arg-i) (ast-get ast '("*")))))
    (let ((variable-uri (find-variable (ast-value param-uri "name") param-uri))
          found-path
          host)
      (labels ((find-uri-components-builder (ast path)
                 (let ((fq-name (find-fq-name-for-reference ast path index)))
                   (unless fq-name (return-from find-uri-components-builder))
                   (cond
                     ((equal
                        fq-name
                        "org.springframework.web.util.UriComponentsBuilder.path-java.lang.String")
                      (setf found-path (format nil "/{~a}"
                                               (convert-to-json-type
                                                 (find-variable-fq-class-name
                                                   (ast-value (get-parameter 0 ast) "name")
                                                   ast path index)))))
                     ((equal
                        fq-name
                        "org.springframework.web.util.UriComponentsBuilder.fromUriString-java.lang.String")
                      (let ((v (find-variable (ast-value (get-parameter 0 ast) "name") ast)))
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
                                        path)))))))))
                   (find-uri-components-builder
                     (first (ast-get ast '("MEMBER_SELECT" "METHOD_INVOCATION")))
                     path))))
        (find-uri-components-builder
          (first (ast-get variable-uri '("METHOD_INVOCATION")))
          path))
      `((:host . ,host)
        (:path . ,found-path)))))

(defun convert-to-json-type (type)
  (alexandria:switch (type :test #'equal)
    ("java.lang.String" "string")
    ("INT" "number")))

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

