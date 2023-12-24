(defpackage #:inga/traversal/spring-kotlin
  (:use #:cl
        #:inga/traversal/base
        #:inga/traversal/kotlin
        #:inga/traversal/spring-base))
(in-package #:inga/traversal/spring-kotlin)

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

