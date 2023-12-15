(defpackage #:inga/traversal/spring-java
  (:use #:cl
        #:inga/traversal/spring-base))
(in-package #:inga/traversal/spring-java)

(defmethod get-value-from-request-mapping ((type (eql :java)) ast)
  (if (trav:ast-value ast "children")
      (or (trav:ast-value
            (first (trav:get-asts ast '("STRING_LITERAL")))
            "name")
          (when (trav:filter-by-names
                  (trav:get-asts ast '("ASSIGNMENT" "IDENTIFIER"))
                  '("value" "path"))
            (trav:ast-value
              (first (trav:get-asts ast '("ASSIGNMENT" "STRING_LITERAL")))
              "name")))
      ""))

(defmethod get-method-from-request-mapping ((type (eql :java)) ast)
  (when (equal (trav:ast-value ast "name") "RequestMapping")
    (let ((method (first (trav:filter-by-name
                           (trav:get-asts ast '("ASSIGNMENT" "IDENTIFIER"))
                           "method"))))
      (trav:ast-value (first (trav:get-asts method '("MEMBER_SELECT") :direction :horizontal))
                      "name"))))

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

