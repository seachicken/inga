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
  (filter-by-name
    (get-asts ast '("IMPORT_LIST" "IMPORT_DIRECTIVE"))
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
      (let ((annotations (get-asts ast '("MODIFIER_LIST" "ANNOTATION_ENTRY"))))
        (unless (filter-by-name (get-asts annotations '("CONSTRUCTOR_CALLEE"
                                                        "TYPE_REFERENCE"
                                                        "USER_TYPE"
                                                        "REFERENCE_EXPRESSION"))
                                "RestController")
          (return-from find-definitions-generic file-definitions))

        (let ((request-mapping
                (first (filter-by-name (get-asts annotations '("CONSTRUCTOR_CALLEE"
                                                               "TYPE_REFERENCE"
                                                               "USER_TYPE"
                                                               "REFERENCE_EXPRESSION"))
                                       "RequestMapping"))))
          (when request-mapping
            (setf rest-base-path
                  (ast-value (first (get-asts 
                                      (ast-value
                                        (first (get-asts request-mapping '("USER_TYPE"
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
      (let* ((mapping (first (get-asts ast '("MODIFIER_LIST" "ANNOTATION_ENTRY"))))
             (rest-paths (mapcar (lambda (v) (merge-paths rest-base-path v))
                                 (get-values-from-request-mapping :kotlin mapping)))
             (file-pos (find-if (lambda (def) (eq (cdr (assoc :top-offset def))
                                                  (jsown:val (ast-value ast "textRange")
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
                                     (first (get-asts
                                              (find-param-from-path-variable
                                                :kotlin (first (get-asts ast '("VALUE_PARAMETER_LIST")))
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
        (lambda (ast) (ast-value ast "name"))
        (or (get-asts ast '("VALUE_ARGUMENT_LIST"
                            "VALUE_ARGUMENT"
                            "STRING_TEMPLATE"
                            "LITERAL_STRING_TEMPLATE_ENTRY"))
            (let* ((value (filter-by-names
                            (get-asts ast '("VALUE_ARGUMENT_LIST"
                                            "VALUE_ARGUMENT"
                                            "VALUE_ARGUMENT_NAME"
                                            "REFERENCE_EXPRESSION"))
                            '("value" "path")))
                   (parent (first (get-asts
                                    value
                                    '("VALUE_ARGUMENT_NAME") :direction :upward))))
              (apply #'append
                     (mapcar
                       (lambda (v) (get-asts v '("LITERAL_STRING_TEMPLATE_ENTRY")))
                       (or (get-asts parent '("STRING_TEMPLATE") :direction :horizontal)
                           (get-asts
                             (first (get-asts
                                      parent
                                      '("COLLECTION_LITERAL_EXPRESSION") :direction :horizontal))
                             '("STRING_TEMPLATE"))))))))
      '("")))

(defmethod get-method-from-request-mapping ((type (eql :kotlin)) ast)
  (let* ((mapping (first (get-asts ast '("CONSTRUCTOR_CALLEE"
                                         "TYPE_REFERENCE"
                                         "USER_TYPE"
                                         "REFERENCE_EXPRESSION"))))
         (method (first (filter-by-name
                          (get-asts ast '("VALUE_ARGUMENT_LIST"
                                          "VALUE_ARGUMENT"
                                          "VALUE_ARGUMENT_NAME"
                                          "REFERENCE_EXPRESSION"))
                          "method")))
         (parent (first (get-asts method 
                                  '("VALUE_ARGUMENT_NAME"
                                    "VALUE_ARGUMENT")
                                  :direction :upward))))
    (if (equal (ast-value mapping "name") "RequestMapping")
        (ast-value (second (get-asts parent '("COLLECTION_LITERAL_EXPRESSION"
                                              "DOT_QUALIFIED_EXPRESSION"
                                              "REFERENCE_EXPRESSION")))
                   "name")
        (to-http-method (ast-value mapping "name")))))

(defmethod find-param-from-path-variable ((type (eql :kotlin)) ast target-name)
  (loop for param in (get-asts ast '("VALUE_PARAMETER"))
        do
        (let* ((pv (filter-by-name
                     (get-asts param '("MODIFIER_LIST"
                                       "ANNOTATION_ENTRY"
                                       "CONSTRUCTOR_CALLEE"
                                       "TYPE_REFERENCE"
                                       "USER_TYPE"
                                       "REFERENCE_EXPRESSION"))
                     "PathVariable"))
               (values (get-asts param '("MODIFIER_LIST"
                                         "ANNOTATION_ENTRY"
                                         "VALUE_ARGUMENT_LIST"
                                         "VALUE_ARGUMENT"))))
          (when pv
            (return
              (if values
                  (loop for value in values
                        do
                        (when (equal (ast-value
                                       (first (get-asts value '("STRING_TEMPLATE"
                                                                "LITERAL_STRING_TEMPLATE_ENTRY")))
                                       "name")
                                     target-name)
                          (return param)))
                  (when (equal (ast-value param "name") target-name)
                    param)))))))

(defmethod find-reference :around ((traversal traversal-kotlin) target-pos fq-name ast path)
  (if (eq (cdr (assoc :type target-pos)) :rest-server)
      (let ((pos (find-if (lambda (rest-client)
                            (and (or (null (cdr (assoc :host rest-client)))
                                     (equal (cdr (assoc :host rest-client))
                                            (cdr (assoc :host target-pos))))
                                 (equal (cdr (assoc :path rest-client))
                                        (cdr (assoc :path target-pos)))
                                 (equal (cdr (assoc :name rest-client))
                                        (cdr (assoc :name target-pos)))))
                          (find-rest-clients traversal fq-name ast path))))
        (when pos
          `((:path . ,(cdr (assoc :path (cdr (assoc :file-pos pos)))))
            (:top-offset . ,(cdr (assoc :top-offset (cdr (assoc :file-pos pos))))))))
      (call-next-method)))

(defmethod find-rest-clients ((traversal traversal-kotlin) fq-name ast path)
  (let ((matched-api (find-signature
                       fq-name
                       #'(lambda (fqcn) (remove-if (lambda (a) (not (assoc :call-type a)))
                                                   (gethash :spring *rest-client-apis*)))
                       path)))
    (when matched-api
      (mapcar (lambda (server)
                `((:host . ,(cdr (assoc :host server)))
                  (:path . ,(cdr (assoc :path server)))
                  (:name . ,(cdr (assoc :method server)))
                  (:file-pos . ((:path . ,path)
                                (:name . ,(ast-value ast "name"))
                                (:top-offset . ,(ast-value ast "textOffset"))))))
              (find-servers ast path)))))

(defun find-servers (ast path)
  (let (server-host server-method server-paths)
    (multiple-value-bind (caller api) (find-caller
                                        (remove-if (lambda (a) (not (assoc :host-i a)))
                                                   (gethash :spring *rest-client-apis*))
                                        ast path)
      (when caller
        (let* ((param (get-parameter (cdr (assoc :host-i api)) caller))
               (host (get-host param)))
          (setf server-host host))))
    (multiple-value-bind (caller api) (find-caller
                                        (remove-if (lambda (a) (and (not (assoc :method a))
                                                                    (not (assoc :method-i a))))
                                                   (gethash :spring *rest-client-apis*))
                                        ast path)
      (when caller
        (setf server-method (get-method api caller))))
    (multiple-value-bind (caller api) (find-caller
                                        (remove-if (lambda (a) (not (assoc :path-i a)))
                                                   (gethash :spring *rest-client-apis*))
                                        ast path)
      (when caller
        (let* ((param (get-parameter (cdr (assoc :path-i api)) caller))
               (literal-poss (find-reference-to-literal param path))
               (host (get-host param)))
          (when host (setf server-host host))
          (setf server-paths (if literal-poss
                                 (mapcar (lambda (p)
                                           (quri:uri-path (quri:uri (cdr (assoc :name p)))))
                                         literal-poss)
                                 (list (format nil "/{~a}"
                                               (convert-to-json-type
                                                 (find-fq-class-name param path)))))))))
    (mapcar (lambda (server-path)
              `((:host . ,server-host)
                (:method . ,server-method)
                (:path . ,server-path)))
            server-paths)))

(defun get-host (ast)
  (let* ((literal (first (get-asts ast '("LITERAL_STRING_TEMPLATE_ENTRY"))))
         (port (when literal (quri:uri-port (quri:uri (ast-value literal "name"))))))
    (when port (format nil "~a" port))))

(defun get-method (api ast)
  (if (assoc :method api)
      (cdr (assoc :method api))
      (find-api-method-from-http-method (get-parameter (cdr (assoc :method-i api)) ast))))

(defun find-api-method-from-http-method (http-method)
  (ast-value (nth 1 (get-asts http-method '("REFERENCE_EXPRESSION"))) "name"))

(defun find-api-host (arg-i ast)
  (let ((url (first (get-asts (get-parameter arg-i ast) '("LITERAL_STRING_TEMPLATE_ENTRY")))))
    (when url
      (format nil "~a" (quri:uri-port (quri:uri (ast-value url "name")))))))

(defun get-parameter (idx ast)
  (nth idx (get-asts ast '("VALUE_ARGUMENT_LIST" "VALUE_ARGUMENT" "*"))))

