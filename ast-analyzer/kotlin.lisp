(defpackage #:inga/ast-analyzer/kotlin
  (:use #:cl
        #:inga/ast-analyzer/base
        #:inga/utils)
  (:import-from #:quri)
  (:import-from #:inga/ast-index
                #:ast-index-paths
                #:ast-index-root-path
                #:clean-indexes
                #:create-indexes
                #:get-ast)
  (:import-from #:inga/file
                #:get-file-type)
  (:export #:ast-analyzer-kotlin))
(in-package #:inga/ast-analyzer/kotlin)

(defvar *package-index-groups* nil)
(defvar *project-index-groups* nil)

(defparameter *include-kotlin* '("*.kt"))

(defclass ast-analyzer-kotlin (ast-analyzer)
  ())

(defmethod start-ast-analyzer ((kind (eql :kotlin)) include exclude path index)
  (setf *ast-analyzers*
        (acons :kotlin
               (make-instance 'ast-analyzer-kotlin
                              :path path
                              :index index)
               *ast-analyzers*))
  (create-indexes index include *include-kotlin* exclude)
  (cdr (assoc :kotlin *ast-analyzers*)))

(defmethod stop-ast-analyzer ((ast-analyzer ast-analyzer-kotlin))
  (clean-indexes (ast-analyzer-index ast-analyzer))
  (setf *ast-analyzers* nil))

(defmethod find-definitions-generic ((ast-analyzer ast-analyzer-kotlin) range)
  (let ((q (make-queue))
        (src-path (cdr (assoc :path range)))
        (path (cdr (assoc :path range)))
        (start-offset (cdr (assoc :start-offset range)))
        (end-offset (cdr (assoc :end-offset range)))
        ast
        results)
    (enqueue q (get-ast (ast-analyzer-index ast-analyzer) path))
    (loop
      (setf ast (dequeue q))
      (if (null ast) (return))

      (when (and
              (equal (ast-value ast "type") "FUN")
              (contains-offset (jsown:val (jsown:val ast "textRange") "startOffset")
                               (jsown:val (jsown:val ast "textRange") "endOffset")            
                               start-offset
                               end-offset))
        (when (jsown:keyp ast "name")
          (setf results
                (append results
                        (list
                          (let ((pos (list
                                       (cons :path src-path)
                                       (cons :name (jsown:val ast "name"))
                                       (cons :fq-name (when (jsown:keyp ast "fqName") (jsown:val ast "fqName")))
                                       (cons :top-offset (jsown:val (jsown:val ast "textRange") "startOffset")))))
                            (when (assoc :origin range)
                              (push (cons :origin (cdr (assoc :origin range))) pos))
                            pos))))))

      (loop for child in (jsown:val ast "children") do (enqueue q child)))
    results))

(defmethod find-reference ((ast-analyzer ast-analyzer-kotlin) target-pos ast path)
  (let ((fq-name (find-fq-name-for-reference ast path (ast-analyzer-index ast-analyzer))))
    (unless fq-name (return-from find-reference))
    (format t "fq-name: ~a~%" fq-name)

    (alexandria:switch ((cdr (assoc :type target-pos)))
      (:rest-server
        (let ((rest-client (find-rest-client fq-name ast path (ast-analyzer-index ast-analyzer))))
          (format t "rest-client: ~a~%" rest-client)
          (when (and
                  (equal (cdr (assoc :host rest-client)) (cdr (assoc :host target-pos)))
                  (equal (cdr (assoc :path rest-client)) (cdr (assoc :path target-pos)))
                  (equal (cdr (assoc :name rest-client)) (cdr (assoc :name target-pos))))
            `((:path . ,path)
              (:top-offset . ,(ast-value ast "textOffset"))))))
      (t
        (when (equal fq-name (cdr (assoc :fq-name target-pos)))
          (list
            (cons :path path)
            (cons :top-offset (ast-value ast "textOffset"))))))))

(defun find-fq-name-for-reference (ast path index)
  (alexandria:switch ((ast-value ast "type") :test #'equal)
    ("CALL_EXPRESSION"
     (let ((root (first (ast-get ast '("DOT_QUALIFIED_EXPRESSION") :direction :upward))))
       (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
               (if (ast-get root '("DOT_QUALIFIED_EXPRESSION"))
                   (let (fq-class-names)
                     ;; get class name
                     (push (ast-value (first (ast-get root '("DOT_QUALIFIED_EXPRESSION"
                                                             "CALL_EXPRESSION"
                                                             "REFERENCE_EXPRESSION")))
                                      "name")
                           fq-class-names)
                     ;; get package name
                     (labels ((get-names (nodes)
                                (mapcar (lambda (ast) (ast-value ast "name")) nodes))
                              (get-package-names (ast)
                                (loop for child in (ast-get ast '("DOT_QUALIFIED_EXPRESSION"))
                                      with names
                                      do
                                      (setf names (append names (get-package-names child)))
                                      (setf names (append names (get-names
                                                                  (ast-get
                                                                    child
                                                                    '("REFERENCE_EXPRESSION")))))
                                      finally (return names))))
                       (setf fq-class-names (append (get-package-names root) fq-class-names)))
                     (format nil "~{~a~^.~}" fq-class-names))
                   (find-fq-class-name
                     (if (> (length (ast-get root '("CALL_EXPRESSION"))) 1)
                         (ast-value
                           (first (ast-get root '("CALL_EXPRESSION" "REFERENCE_EXPRESSION")))
                           "name") 
                         (find-variable-name
                           (ast-value (first (ast-get root '("REFERENCE_EXPRESSION"))) "name")
                           ast))
                     ast))
               (ast-value (first (ast-get ast '("REFERENCE_EXPRESSION"))) "name")
               (mapcar (lambda (arg) (find-fq-class-name-by-ast arg path index))
                       (ast-get ast '("VALUE_ARGUMENT_LIST" "VALUE_ARGUMENT" "*"))))))))

(defun find-fq-class-name-by-ast (ast path index)
  (alexandria:switch ((ast-value ast "type") :test #'equal)
    ("STRING_TEMPLATE"
     "java.lang.String")
    ("NULL"
     "NULL")
    ("DOT_QUALIFIED_EXPRESSION"
     (cond
       ((ast-get ast '("CLASS_LITERAL_EXPRESSION"))
        "java.lang.Class")
       ((ast-get ast '("REFERENCE_EXPRESSION"))
        (find-fq-class-name
          (ast-value (first (ast-get ast '("REFERENCE_EXPRESSION"))) "name")
          ast))))))

(defun find-fq-class-name (class-name ast)
  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (when (null ast) (return))

    (when (equal (ast-value ast "type") "kotlin.FILE")
      (let ((import (first (ast-find-suffix
                             (ast-get ast '("IMPORT_LIST" "IMPORT_DIRECTIVE"))
                             (concatenate 'string "." class-name)
                             :key-name "fqName"))))
        (when import
          (return (ast-value import "fqName")))))

    (enqueue q (ast-value ast "parent"))))

(defun find-variable-name (object-name ast)
  (unless object-name
    (return-from find-variable-name))

  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (when (null ast) (return))

    (when (equal (ast-value ast "type") "CLASS")
      (let ((variable (first (ast-find-name (ast-get ast '("PRIMARY_CONSTRUCTOR"
                                                           "VALUE_PARAMETER_LIST"
                                                           "VALUE_PARAMETER"))
                                            object-name))))
        (return-from find-variable-name
                     (ast-value (first (ast-get variable '("TYPE_REFERENCE"
                                                           "USER_TYPE"
                                                           "REFERENCE_EXPRESSION")))
                                "name"))))

    (enqueue q (ast-value ast "parent"))))

(defun find-rest-client (fq-name ast path index)
  ;; https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
  (cond
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-NULL-java.lang.Class"
       index)
     `((:host . ,(find-api-host 0 ast))
       (:name . ,(find-api-method-from-http-method (get-parameter 1 ast)))
       (:path . ,(find-api-path 0 ast))))
    ((matches-signature
       fq-name
       "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-java.lang.Class"
       index)
     `((:host . ,(find-api-host 0 ast))
       (:name . "GET")
       (:path . ,(find-api-path 0 ast))))))

(defun find-api-method-from-http-method (http-method)
  (ast-value (nth 1 (ast-get http-method '("REFERENCE_EXPRESSION"))) "name"))

(defun find-api-host (arg-i ast)
  (let ((url (first (ast-get (get-parameter arg-i ast) '("LITERAL_STRING_TEMPLATE_ENTRY")))))
    (when url
      (format nil "~a" (quri:uri-port (quri:uri (ast-value url "name")))))))

(defun find-api-path (arg-i ast)
  (let ((url (first (ast-get (get-parameter arg-i ast) '("LITERAL_STRING_TEMPLATE_ENTRY")))))
    (when url
      (quri:uri-path (quri:uri (ast-value url "name"))))))

(defun get-parameter (idx ast)
  (nth idx (ast-get ast '("VALUE_ARGUMENT_LIST" "VALUE_ARGUMENT" "*"))))

