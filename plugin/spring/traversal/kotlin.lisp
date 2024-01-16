(defpackage #:inga/plugin/spring/traversal/kotlin
  (:use #:cl
        #:inga/plugin/spring/traversal/base
        #:inga/traversal
        #:inga/utils)
  (:import-from #:quri)
  (:import-from #:inga/ast-index
                #:get-ast)
  (:import-from #:inga/path
                #:get-variable-names
                #:merge-paths  
                #:replace-variable-name)
  (:import-from #:inga/logger
                #:log-error)
  (:import-from #:inga/plugin/jvm-helper
                #:convert-to-json-type)
  (:import-from #:inga/plugin/spring/spring-property-loader
                #:find-property))
(in-package #:inga/plugin/spring/traversal/kotlin)

(defmethod set-index-group :after ((traversal traversal-kotlin) path)
  (when (is-rest-client (get-ast (traversal-index traversal) path))
    (push path (gethash :rest-client *file-index*))))

(defun is-rest-client (ast)
  (trav:filter-by-name
    (trav:get-asts ast '("IMPORT_LIST" "IMPORT_DIRECTIVE"))
    "org.springframework.web.client.RestTemplate"
    :key-name "fqName"))

(defmethod get-scoped-index-paths-generic :around ((traversal traversal-kotlin) pos)
  (if (eq (cdr (assoc :type pos)) :rest-server)
      (gethash :rest-client *file-index*)
      (call-next-method)))

(defmethod find-definitions-generic :around ((traversal traversal-kotlin) range)
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
      (let ((annotations (trav:get-asts ast '("MODIFIER_LIST" "ANNOTATION_ENTRY"))))

        (unless (trav:filter-by-name (trav:get-asts annotations '("CONSTRUCTOR_CALLEE"
                                                                  "TYPE_REFERENCE"
                                                                  "USER_TYPE"
                                                                  "REFERENCE_EXPRESSION"))
                                     "RestController")
          (return-from find-definitions-generic file-definitions))

        (let ((request-mapping
                (first (trav:filter-by-name (trav:get-asts annotations '("CONSTRUCTOR_CALLEE"
                                                                         "TYPE_REFERENCE"
                                                                         "USER_TYPE"
                                                                         "REFERENCE_EXPRESSION"))
                                            "RequestMapping"))))
          (when request-mapping
            (setf rest-base-path
                  (ast-value (first (trav:get-asts 
                                      (ast-value
                                        (first (trav:get-asts request-mapping '("USER_TYPE"
                                                                                "TYPE_REFERENCE"
                                                                                "CONSTRUCTOR_CALLEE")
                                                              :direction :upward))
                                        "parent")
                                      '("VALUE_ARGUMENT_LIST"
                                        "VALUE_ARGUMENT"
                                        "STRING_TEMPLATE"
                                        "LITERAL_STRING_TEMPLATE_ENTRY")))
                             "name"))))))

    (when (and (equal (ast-value ast "type") "FUN")
               (contains-offset (jsown:val (jsown:val ast "textRange") "startOffset")
                                (jsown:val (jsown:val ast "textRange") "endOffset")            
                                start-offset
                                end-offset))
      (let* ((mapping (first (trav:get-asts ast '("MODIFIER_LIST"
                                                  "ANNOTATION_ENTRY"))))
             (rest-paths (mapcar (lambda (v) (merge-paths rest-base-path v))
                                 (get-values-from-request-mapping :kotlin mapping)))
             (file-pos (find-if (lambda (def) (eq (cdr (assoc :top-offset def))
                                                  (jsown:val (trav:ast-value ast "textRange")
                                                             "startOffset")))
                                file-definitions)))
        (unless mapping
          (return-from find-definitions-generic file-definitions))

        (labels ((to-general-path (rest-path)
                   (loop for vn in (get-variable-names rest-path)
                         with result = rest-path
                         do
                         (setf result
                               (replace-variable-name
                                 result vn
                                 (convert-to-json-type
                                   (find-fq-class-name
                                     (first (trav:get-asts
                                              (find-param-from-path-variable
                                                :kotlin (first (trav:get-asts ast '("VALUE_PARAMETER_LIST")))
                                                vn)
                                              '("TYPE_REFERENCE")))
                                     path))))
                         finally (return result))))
          (return (mapcar
                    (lambda (rest-path)
                      `((:type . :rest-server)
                        (:host . ,(find-property "server.port" path))
                        (:name . ,(get-method-from-request-mapping :kotlin mapping))
                        (:path . ,(to-general-path rest-path))
                        (:origin . ,(cdr (assoc :origin file-pos)))
                        (:file-pos . ,file-pos)))
                    rest-paths)))))

    (loop for child in (jsown:val ast "children") do (enqueue q child))))

(defmethod get-values-from-request-mapping ((type (eql :kotlin)) ast)
  (or (mapcar
        (lambda (ast) (trav:ast-value ast "name"))
        (or (trav:get-asts ast '("VALUE_ARGUMENT_LIST"
                                 "VALUE_ARGUMENT"
                                 "STRING_TEMPLATE"
                                 "LITERAL_STRING_TEMPLATE_ENTRY"))
            (let* ((value (trav:filter-by-names
                            (trav:get-asts ast '("VALUE_ARGUMENT_LIST"
                                                 "VALUE_ARGUMENT"
                                                 "VALUE_ARGUMENT_NAME"
                                                 "REFERENCE_EXPRESSION"))
                            '("value" "path")))
                   (parent (first (trav:get-asts
                                    value
                                    '("VALUE_ARGUMENT_NAME") :direction :upward))))
              (apply #'append
                     (mapcar
                       (lambda (v) (trav:get-asts v '("LITERAL_STRING_TEMPLATE_ENTRY")))
                       (or (trav:get-asts parent
                                          '("STRING_TEMPLATE") :direction :horizontal)
                           (trav:get-asts
                             (first (trav:get-asts
                                      parent
                                      '("COLLECTION_LITERAL_EXPRESSION") :direction :horizontal))
                             '("STRING_TEMPLATE"))))))))
      '("")))

(defmethod get-method-from-request-mapping ((type (eql :kotlin)) ast)
  (let* ((mapping (first (trav:get-asts ast '("CONSTRUCTOR_CALLEE"
                                              "TYPE_REFERENCE"
                                              "USER_TYPE"
                                              "REFERENCE_EXPRESSION"))))
         (method (first (trav:filter-by-name
                          (trav:get-asts ast '("VALUE_ARGUMENT_LIST"
                                               "VALUE_ARGUMENT"
                                               "VALUE_ARGUMENT_NAME"
                                               "REFERENCE_EXPRESSION"))
                          "method")))
         (parent (first (trav:get-asts
                          method 
                          '("VALUE_ARGUMENT_NAME"
                            "VALUE_ARGUMENT") :direction :upward))))
    (if (equal (trav:ast-value mapping "name") "RequestMapping")
        (trav:ast-value (second (trav:get-asts parent '("DOT_QUALIFIED_EXPRESSION"
                                                        "REFERENCE_EXPRESSION")))
                        "name")
        (to-http-method (trav:ast-value mapping "name")))))

(defmethod find-param-from-path-variable ((type (eql :kotlin)) ast target-name)
  (loop for param in (trav:get-asts ast '("VALUE_PARAMETER"))
        do
        (let* ((pv (trav:filter-by-name
                     (trav:get-asts param '("MODIFIER_LIST"
                                            "ANNOTATION_ENTRY"
                                            "CONSTRUCTOR_CALLEE"
                                            "TYPE_REFERENCE"
                                            "USER_TYPE"
                                            "REFERENCE_EXPRESSION"))
                     "PathVariable"))
               (values (trav:get-asts param '("MODIFIER_LIST"
                                              "ANNOTATION_ENTRY"
                                              "VALUE_ARGUMENT_LIST"
                                              "VALUE_ARGUMENT"))))
          (when pv
            (return
              (if values
                  (loop for value in values
                        do
                        (when (equal (trav:ast-value
                                       (first (trav:get-asts value '("STRING_TEMPLATE"
                                                                     "LITERAL_STRING_TEMPLATE_ENTRY")))
                                       "name")
                                     target-name)
                          (return param)))
                  (when (equal (trav:ast-value param "name") target-name)
                    param)))))))

(defmethod find-reference :around ((traversal traversal-kotlin) target-pos fq-name ast path)
  (if (eq (cdr (assoc :type target-pos)) :rest-server)
      (or (let ((pos (find-if (lambda (rest-client)
                                (and (equal (cdr (assoc :host rest-client))
                                            (cdr (assoc :host target-pos)))
                                     (equal (cdr (assoc :path rest-client))
                                            (cdr (assoc :path target-pos)))
                                     (equal (cdr (assoc :name rest-client))
                                            (cdr (assoc :name target-pos)))))
                              (find-rest-clients traversal fq-name ast path))))
            (when pos
              `((:path . ,(cdr (assoc :path (cdr (assoc :file-pos pos)))))
                (:top-offset . ,(cdr (assoc :top-offset (cdr (assoc :file-pos pos))))))))
          (let ((rest-client (find-rest-client fq-name ast (traversal-index traversal))))
            (when (and
                    (equal (cdr (assoc :host rest-client)) (cdr (assoc :host target-pos)))
                    (equal (cdr (assoc :path rest-client)) (cdr (assoc :path target-pos)))
                    (equal (cdr (assoc :name rest-client)) (cdr (assoc :name target-pos))))
              `((:path . ,path)
                (:top-offset . ,(ast-value ast "textOffset"))))))
      (call-next-method)))

(defmethod find-rest-clients ((traversal traversal-kotlin) fq-name ast path)
  (let* ((rest-template-apis (gethash :rest-template *rest-client-apis*))
         (matched-api (find-if (lambda (api)
                                 (matches-signature fq-name
                                                    (cdr (assoc :fq-name api))
                                                    (traversal-index traversal)))
                               rest-template-apis)))
    (when matched-api
      (mapcar (lambda (pos)
                `((:host . ,(find-api-host 0 ast))
                  (:path . ,(quri:uri-path (quri:uri (cdr (assoc :name pos)))))
                  (:name . ,(find-api-method-from-http-method
                              (get-parameter (cdr (assoc :method-i matched-api)) ast)))
                  (:file-pos . ,pos)))
              (find-reference-to-literal (get-parameter (cdr (assoc :path-i matched-api)) ast) path)))))

;; TODO: remove this function
;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
(defun find-rest-client (fq-name ast index)
  (cond
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-java.lang.Class"
       index)
     `((:host . ,(find-api-host 0 ast))
       (:name . "GET")
       (:path . ,(find-api-path 0 ast))))
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
  (ast-value (nth 1 (trav:get-asts http-method '("REFERENCE_EXPRESSION"))) "name"))

(defun find-api-host (arg-i ast)
  (let ((url (first (trav:get-asts (get-parameter arg-i ast) '("LITERAL_STRING_TEMPLATE_ENTRY")))))
    (when url
      (format nil "~a" (quri:uri-port (quri:uri (ast-value url "name")))))))

(defun find-api-path (arg-i ast)
  (let ((url (first (trav:get-asts (get-parameter arg-i ast) '("LITERAL_STRING_TEMPLATE_ENTRY")))))
    (when url
      (quri:uri-path (quri:uri (ast-value url "name"))))))

(defun get-parameter (idx ast)
  (nth idx (trav:get-asts ast '("VALUE_ARGUMENT_LIST" "VALUE_ARGUMENT" "*"))))

