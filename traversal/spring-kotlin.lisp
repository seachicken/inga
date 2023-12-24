(defpackage #:inga/traversal/spring-kotlin
  (:use #:cl
        #:inga/traversal/base
        #:inga/traversal/kotlin
        #:inga/traversal/spring-base
        #:inga/utils)
  (:import-from #:inga/ast-index
                #:get-ast)
  (:import-from #:inga/path
                #:get-variable-names
                #:merge-paths  
                #:replace-variable-name)
  (:import-from #:inga/plugin/jvm-helper
                #:convert-to-json-type)
  (:import-from #:inga/plugin/spring-property-loader
                #:find-property))
(in-package #:inga/traversal/spring-kotlin)

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
                                 (get-values-from-request-mapping :kotlin mapping))))
        (unless mapping
          (return-from find-definitions-generic file-definitions))

        (labels ((to-general-path (path)
                   (loop for vn in (get-variable-names path)
                         with result = path
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
                                              '("TYPE_REFERENCE")))))))
                         finally (return result))))
          (return (mapcar
                    (lambda (rest-path)
                      `((:type . :rest-server)
                        (:host . ,(find-property "server.port" path))
                        (:name . ,(get-method-from-request-mapping :kotlin mapping))
                        (:path . ,(to-general-path rest-path))
                        (:file-pos .
                         ,(find-if (lambda (def) (eq (cdr (assoc :top-offset def))
                                                     (jsown:val (trav:ast-value ast "textRange")
                                                                "startOffset")))
                                   file-definitions))))
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

