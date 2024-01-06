(defpackage #:inga/traversal/spring-java
  (:use #:cl
        #:inga/traversal/base
        #:inga/traversal/java
        #:inga/traversal/spring-base
        #:inga/utils)
  (:import-from #:quri)
  (:import-from #:inga/ast-index
                #:get-ast)
  (:import-from #:inga/logger
                #:log-error)
  (:import-from #:inga/path
                #:get-variable-names
                #:merge-paths  
                #:replace-variable-name)
  (:import-from #:inga/plugin/jvm-helper
                #:convert-to-json-type)
  (:import-from #:inga/plugin/spring-property-loader
                #:find-property))
(in-package #:inga/traversal/spring-java)

(defmethod set-index-group :after ((traversal traversal-java) path)
  (when (is-rest-client (get-ast (traversal-index traversal) path))
    (push path (gethash :rest-client *file-index*))))

(defun is-rest-client (ast)
  (trav:filter-by-name
    (trav:get-asts ast '("COMPILATION_UNIT" "IMPORT"))
    "org.springframework.web.client.RestTemplate"
    :key-name "fqName"))

(defmethod get-scoped-index-paths-generic :around ((traversal traversal-java) pos)
  (if (eq (cdr (assoc :type pos)) :rest-server)
      (gethash :rest-client *file-index*)
      (call-next-method)))

(defmethod find-definitions-generic :around ((traversal traversal-java) range)
  (loop
    with q = (make-queue)
    with path = (cdr (assoc :path range))
    with start-offset = (cdr (assoc :start-offset range))
    with end-offset = (cdr (assoc :end-offset range))
    with ast = (get-ast (traversal-index traversal) path)
    with file-definitions = (call-next-method)
    with rest-base-path = ""
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (unless ast (return))

    (when (or (equal (ast-value ast "type") "CLASS")
              (equal (ast-value ast "type") "INTERFACE"))
      (let ((annotations (trav:get-asts ast '("MODIFIERS" "ANNOTATION"))))
        (unless (trav:filter-by-name annotations "RestController")
          (return file-definitions))

        (let ((request-mapping (first (trav:filter-by-name annotations "RequestMapping"))))
          (when request-mapping
            (setf rest-base-path
                  (or
                    (ast-value
                      (first (trav:get-asts request-mapping '("STRING_LITERAL")))
                      "name")
                    (ast-value
                      (first (trav:get-asts request-mapping '("ASSIGNMENT" "STRING_LITERAL")))
                      "name")))))))

    (when (and (equal (ast-value ast "type") "METHOD")
               (contains-offset (jsown:val ast "startPos") (jsown:val ast "endPos")
                                start-offset end-offset))
      (let* ((mapping (first (trav:filter-by-names
                               (trav:get-asts ast '("MODIFIERS" "ANNOTATION"))
                               '("GetMapping" "PostMapping" "PutMapping" "DeleteMapping"
                                 "RequestMapping"))))
             (rest-paths (mapcar (lambda (v) (merge-paths rest-base-path v))
                            (get-values-from-request-mapping :java mapping)))
             (file-pos (find-if (lambda (def) (eq (cdr (assoc :top-offset def))
                                                  (trav:ast-value ast "pos")))
                                file-definitions)))
        (unless mapping
          (return file-definitions))

        (labels ((to-general-path (rest-path)
                   (loop for vn in (get-variable-names rest-path)
                         with result = rest-path
                         do
                         (setf result
                               (replace-variable-name
                                  result vn
                                  (convert-to-json-type
                                    (find-fq-class-name
                                      (find-param-from-path-variable :java ast vn)
                                      path))))
                         finally (return result))))
          (return (mapcar
                    (lambda (rest-path)
                      `((:type . :rest-server)
                        (:host . ,(find-property "server.port" path))
                        (:name . ,(get-method-from-request-mapping :java mapping))
                        (:path . ,(to-general-path rest-path))
                        (:origin . ,(cdr (assoc :origin file-pos)))
                        (:file-pos . ,file-pos)))
                    rest-paths)))))

    (loop for child in (jsown:val ast "children") do (enqueue q child))))

(defmethod find-reference :around ((traversal traversal-java) target-pos fq-name ast path)
  (if (eq (cdr (assoc :type target-pos)) :rest-server)
      (let ((rest-client (find-rest-client fq-name ast path (traversal-index traversal))))
        (when (and
                (equal (cdr (assoc :host rest-client)) (cdr (assoc :host target-pos)))
                (equal (cdr (assoc :path rest-client)) (cdr (assoc :path target-pos)))
                (equal (cdr (assoc :name rest-client)) (cdr (assoc :name target-pos))))
          `((:path . ,path)
            (:top-offset . ,(ast-value ast "startPos")))))
      (call-next-method)))

;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
(defun find-rest-client (fq-name ast path index)
  (cond
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-org.springframework.http.HttpEntity-org.springframework.core.ParameterizedTypeReference-java.lang.Object"
       index)
     `((:host . ,(find-api-host 0 ast))
       (:name . ,(find-api-method-from-http-method (nth 2 (trav:get-asts ast '("*")))))
       (:path . ,(find-api-path 0 ast))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-org.springframework.http.HttpEntity-java.lang.Class"
       index)
     `((:host . ,(find-api-host 0 ast))
       (:name . ,(find-api-method-from-http-method (nth 2 (trav:get-asts ast '("*")))))
       (:path . ,(find-api-path 0 ast))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-java.lang.Class"
       index)
     `((:host . ,(find-api-host 0 ast))
       (:name . "GET")
       (:path . ,(find-api-path 0 ast))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.getForObject-java.net.URI-java.lang.Class"
       index)
     (let ((server (find-server-from-uri 0 ast path)))
       `((:host . ,(cdr (assoc :host server)))
         (:name . "GET")
         (:path . ,(cdr (assoc :path server))))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.postForObject-java.lang.String-java.lang.Object-java.lang.Class"
       index)
     `((:host . ,(find-api-host 0 ast))
       (:name . "POST")
       (:path . ,(find-api-path 0 ast))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.*"
       index)
     (log-error (format nil "unexpected signature of RestTemplate. fq-name: ~a" fq-name)))))

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
  (nth (1+ idx) (trav:get-asts ast '("*"))))

(defun find-server-from-uri (arg-i ast path)
  (let ((param-uri (nth (1+ arg-i) (trav:get-asts ast '("*")))))
    (let ((variable-uri (find-variable (ast-value param-uri "name") param-uri))
          found-path
          host)
      (labels ((find-uri-components-builder (ast path)
                 (let ((fq-name (find-fq-name ast path)))
                   (unless fq-name (return-from find-uri-components-builder))
                   (cond
                     ((equal
                        fq-name
                        "org.springframework.web.util.UriComponentsBuilder.path-java.lang.String")
                      (setf found-path (format nil "/{~a}"
                                               (convert-to-json-type
                                                 (find-fq-class-name (get-parameter 0 ast) path)))))
                     ((equal
                        fq-name
                        "org.springframework.web.util.UriComponentsBuilder.fromUriString-java.lang.String")
                      (let ((v (find-variable (ast-value (get-parameter 0 ast) "name") ast)))
                        (let ((value (first (trav:filter-by-name
                                              (trav:get-asts v '("MODIFIERS"
                                                                 "ANNOTATION"))
                                              "Value"))))
                          (setf host
                                (write-to-string
                                  (quri:uri-port
                                    (quri:uri 
                                      (find-property
                                        (first (get-variable-names
                                                 (ast-value (first (trav:get-asts value '("STRING_LITERAL"))) "name")))
                                        path)))))))))
                   (find-uri-components-builder
                     (first (trav:get-asts ast '("MEMBER_SELECT" "METHOD_INVOCATION")))
                     path))))
        (find-uri-components-builder
          (first (trav:get-asts variable-uri '("METHOD_INVOCATION")))
          path))
      `((:host . ,host)
        (:path . ,found-path)))))

(defmethod get-values-from-request-mapping ((type (eql :java)) ast)
  (let* ((values (trav:filter-by-names
                   (trav:get-asts ast '("ASSIGNMENT" "IDENTIFIER"))
                   '("value" "path")))
         (values (mapcar (lambda (ast) (trav:ast-value ast "name"))
                         (or (trav:get-asts ast '("STRING_LITERAL"))
                             (apply #'append
                                    (mapcar (lambda (v)
                                              (or (trav:get-asts v '("STRING_LITERAL")
                                                                 :direction :horizontal)
                                                  (trav:get-asts
                                                    (first (trav:get-asts v '("NEW_ARRAY")
                                                                          :direction :horizontal))
                                                    '("STRING_LITERAL"))))
                                            values))))))
    (or values '(""))))

(defmethod get-method-from-request-mapping ((type (eql :java)) ast)
  (if (equal (trav:ast-value ast "name") "RequestMapping")
    (let ((method (first (trav:filter-by-name
                           (trav:get-asts ast '("ASSIGNMENT" "IDENTIFIER"))
                           "method"))))
      (trav:ast-value (first (trav:get-asts method '("MEMBER_SELECT") :direction :horizontal))
                      "name"))
    (to-http-method (trav:ast-value ast "name"))))

(defmethod find-param-from-path-variable ((type (eql :java)) ast target-name)
  (loop for param in (nthcdr 1 (trav:get-asts ast '("*")))
        do
        (let* ((pv (trav:filter-by-name
                     (trav:get-asts param '("MODIFIERS" "ANNOTATION"))
                     "PathVariable"))
               (value (first (or (trav:get-asts pv '("STRING_LITERAL"))
                                 (trav:get-asts
                                   (first (trav:filter-by-names
                                            (trav:get-asts pv '("ASSIGNMENT" "IDENTIFIER"))
                                            '("value" "name")))
                                   '("STRING_LITERAL") :direction :horizontal)))))
          (when pv
            (return (if (equal (trav:ast-value value "name") target-name)
                        param
                        (when (equal (trav:ast-value param "name") target-name)
                          param)))))))

