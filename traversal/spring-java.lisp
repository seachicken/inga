(defpackage #:inga/traversal/spring-java
  (:use #:cl
        #:inga/traversal/spring-base))
(in-package #:inga/traversal/spring-java)

(defmethod get-value-from-path-variable ((type (eql :java)) ast)
  (if (trav:ast-value ast "children")
      (or (trav:ast-value
            (first (trav:get-asts ast '("STRING_LITERAL")))
            "name")
          (when (trav:ast-find-names
                  (trav:get-asts ast '("ASSIGNMENT" "IDENTIFIER"))
                  '("value" "name"))
            (trav:ast-value
              (first (trav:get-asts ast '("ASSIGNMENT" "STRING_LITERAL")))
              "name")))
      (trav:ast-value
        (first (trav:get-asts ast '("MODIFIERS" "VARIABLE") :direction :upward))
        "name")))

