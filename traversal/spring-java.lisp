(defpackage #:inga/traversal/spring-java
  (:use #:cl
        #:inga/traversal/spring-base))
(in-package #:inga/traversal/spring-java)

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

(defmethod get-value-from-path-variable ((type (eql :java)) ast)
  (if (trav:ast-value ast "children")
      (or (trav:ast-value
            (first (trav:get-asts ast '("STRING_LITERAL")))
            "name")
          (when (trav:filter-by-names
                  (trav:get-asts ast '("ASSIGNMENT" "IDENTIFIER"))
                  '("value" "name"))
            (trav:ast-value
              (first (trav:get-asts ast '("ASSIGNMENT" "STRING_LITERAL")))
              "name")))
      (trav:ast-value
        (first (trav:get-asts ast '("MODIFIERS" "VARIABLE") :direction :upward))
        "name")))

