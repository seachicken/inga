(defpackage #:inga/traversal/spring-java
  (:use #:cl
        #:inga/traversal/java
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
(in-package #:inga/traversal/spring-java)

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
          (return-from find-definitions-generic file-definitions))
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
                            (get-values-from-request-mapping :java mapping))))
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
                                      (find-if (lambda (param)
                                                 (equal (trav:ast-value param "name") vn))
                                               (nthcdr 1 (trav:get-asts ast '("*"))))
                                      path (traversal-index traversal)))))
                         finally (return result))))
          (return (mapcar
                    (lambda (rest-path)
                      `((:type . :rest-server)
                        (:host . ,(find-property "server.port" path))
                        (:name . ,(get-method-from-request-mapping :java mapping))
                        (:path . ,(to-general-path rest-path))
                        (:file-pos .
                         ,(find-if (lambda (def) (eq (cdr (assoc :top-offset def))
                                                     (trav:ast-value ast "pos")))
                                   file-definitions))))
                    rest-paths)))))
    (loop for child in (jsown:val ast "children") do (enqueue q child))))

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

