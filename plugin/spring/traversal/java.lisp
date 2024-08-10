(defpackage #:inga/plugin/spring/traversal/java
  (:use #:cl
        #:inga/traversal
        #:inga/plugin/spring/traversal/base
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
  (:import-from #:inga/plugin/spring/spring-property-loader
                #:find-property))
(in-package #:inga/plugin/spring/traversal/java)

(defmethod set-index-group :after ((traversal traversal-java) path)
  (when (is-rest-client (get-ast (traversal-index traversal) path))
    (push path (gethash :rest-client *file-index*))))

(defun is-rest-client (ast)
  (filter-by-names
    (get-asts ast '("COMPILATION_UNIT" "IMPORT"))
    '("org.springframework.web.client.RestTemplate"
      "org.springframework.web.reactive.function.client.WebClient")
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
      (let ((annotations (get-asts ast '("MODIFIERS" "ANNOTATION"))))
        (unless (filter-by-name annotations "RestController")
          (return file-definitions))

        (let ((request-mapping (first (filter-by-name annotations "RequestMapping"))))
          (when request-mapping
            (setf rest-base-path
                  (or
                    (ast-value
                      (first (get-asts request-mapping '("STRING_LITERAL")))
                      "name")
                    (ast-value
                      (first (get-asts request-mapping '("ASSIGNMENT" "STRING_LITERAL")))
                      "name")))))))

    (when (and (equal (ast-value ast "type") "METHOD")
               (contains-offset (jsown:val ast "startPos") (jsown:val ast "endPos")
                                start-offset end-offset))
      (let* ((mapping (first (filter-by-names
                               (get-asts ast '("MODIFIERS" "ANNOTATION"))
                               '("GetMapping" "PostMapping" "PutMapping" "DeleteMapping"
                                 "RequestMapping"))))
             (rest-paths (mapcar (lambda (v) (merge-paths rest-base-path v))
                            (get-values-from-request-mapping :java mapping)))
             (file-pos (find-if (lambda (def) (eq (cdr (assoc :top-offset def))
                                                  (ast-value ast "pos")))
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

(defmethod find-rest-clients ((traversal traversal-java) fq-name ast path)
  (let ((matched-api (find-signature
                       fq-name
                       #'(lambda (fqcn) (remove-if (lambda (a) (not (assoc :call-type a)))
                                                   (gethash :spring *rest-client-apis*)))
                       (traversal-index traversal))))
    (when matched-api
      (let ((server (find-server ast path)))
        `(((:host . ,(cdr (assoc :host server)))
           (:path . ,(cdr (assoc :path server)))
           (:name . ,(cdr (assoc :method server)))
           (:file-pos . ((:path . ,path)
                         (:name . ,(ast-value ast "name"))
                         (:top-offset . ,(ast-value ast "pos"))))))))))

(defun find-server (ast path)
  (let (server-host server-method server-path)
    (multiple-value-bind (caller api) (find-caller
                                        (remove-if (lambda (a) (not (assoc :host-i a)))
                                                   (gethash :spring *rest-client-apis*))
                                        ast path)
      (when caller
        (let* ((param (get-parameter (cdr (assoc :host-i api)) caller))
               (host (get-host param)))
          (setf server-host
                (if host
                    host
                    (let* ((v (find-definition (ast-value param "name") ast))
                           (value (first (filter-by-name
                                           (get-asts v '("MODIFIERS" "ANNOTATION"))
                                           "Value"))))
                      (write-to-string
                        (quri:uri-port (quri:uri 
                                         (find-property
                                           (first (get-variable-names
                                                    (ast-value (first (get-asts value '("STRING_LITERAL")))
                                                               "name")))
                                           path))))))))))
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
               (pos (first (find-reference-to-literal param path)))
               (host (get-host param)))
          (when host (setf server-host host))
          (setf server-path (if pos
                                (quri:uri-path (quri:uri (cdr (assoc :name pos))))
                                (format nil "/{~a}"
                                        (convert-to-json-type
                                          (find-fq-class-name param path))))))))
    `((:host . ,server-host)
      (:method . ,server-method)
      (:path . ,server-path))))

(defun get-method (api ast)
  (if (assoc :method api)
      (cdr (assoc :method api))
      (find-api-method-from-http-method (get-parameter (cdr (assoc :method-i api)) ast))))

(defun find-api-method-from-http-method (http-method)
  (ast-value http-method "name"))

(defun find-api-host (arg-i ast)
  (let ((url (get-parameter arg-i ast)))
    (when (equal (ast-value url "type") "STRING_LITERAL")
      (format nil "~a" (quri:uri-port (quri:uri (ast-value url "name")))))))

(defun get-host (ast)
  (when (equal (ast-value ast "type") "STRING_LITERAL")
    (let ((port (quri:uri-port (quri:uri (ast-value ast "name")))))
      (when port (format nil "~a" port)))))

(defun get-parameter (idx ast)
  (nth (1+ idx) (get-asts ast '("*"))))

(defmethod get-values-from-request-mapping ((type (eql :java)) ast)
  (let* ((values (filter-by-names
                   (get-asts ast '("ASSIGNMENT" "IDENTIFIER"))
                   '("value" "path")))
         (values (mapcar (lambda (ast) (ast-value ast "name"))
                         (or (get-asts ast '("STRING_LITERAL"))
                             (apply #'append
                                    (mapcar (lambda (v)
                                              (or (get-asts v '("STRING_LITERAL")
                                                            :direction :horizontal)
                                                  (get-asts
                                                    (first (get-asts v '("NEW_ARRAY")
                                                                     :direction :horizontal))
                                                    '("STRING_LITERAL"))))
                                            values))))))
    (or values '(""))))

(defmethod get-method-from-request-mapping ((type (eql :java)) ast)
  (if (equal (ast-value ast "name") "RequestMapping")
    (let ((method (first (filter-by-name
                           (get-asts ast '("ASSIGNMENT" "IDENTIFIER"))
                           "method"))))
      (ast-value (first (get-asts method '("MEMBER_SELECT") :direction :horizontal)) "name"))
    (to-http-method (ast-value ast "name"))))

(defmethod find-param-from-path-variable ((type (eql :java)) ast target-name)
  (loop for param in (nthcdr 1 (get-asts ast '("*")))
        do
        (let* ((pv (filter-by-name
                     (get-asts param '("MODIFIERS" "ANNOTATION"))
                     "PathVariable"))
               (value (first (or (get-asts pv '("STRING_LITERAL"))
                                 (get-asts
                                   (first (filter-by-names
                                            (get-asts pv '("ASSIGNMENT" "IDENTIFIER"))
                                            '("value" "name")))
                                   '("STRING_LITERAL") :direction :horizontal)))))
          (when pv
            (return (if (equal (ast-value value "name") target-name)
                        param
                        (when (equal (ast-value param "name") target-name)
                          param)))))))

