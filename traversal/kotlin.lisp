(defpackage #:inga/traversal/kotlin
  (:use #:cl
        #:inga/traversal/base
        #:inga/utils)
  (:import-from #:inga/ast-index
                #:ast-index-paths
                #:ast-index-root-path
                #:clean-indexes
                #:create-indexes
                #:get-ast)
  (:import-from #:inga/file
                #:get-file-type)
  (:import-from #:inga/plugin/jvm-dependency-loader
                #:load-hierarchy 
                #:load-signatures)
  (:export #:traversal-kotlin))
(in-package #:inga/traversal/kotlin)

(defparameter *include-kotlin* '("*.kt"))

(defclass traversal-kotlin (traversal)
  ())

(defmethod start-traversal ((kind (eql :kotlin)) include exclude path index)
  (setf *traversals*
        (acons :kotlin
               (make-instance 'traversal-kotlin
                              :path path
                              :index index)
               *traversals*))
  (create-indexes index include *include-kotlin* exclude)
  (create-index-groups (cdr (assoc :kotlin *traversals*)))
  (cdr (assoc :kotlin *traversals*)))

(defmethod stop-traversal ((traversal traversal-kotlin))
  (clean-indexes (traversal-index traversal))
  (setf *traversals* nil))

(defun create-index-groups (traversal)
  (loop for path in (remove-if-not (lambda (p) (eq (get-file-type p) :kotlin))
                                   (ast-index-paths (traversal-index traversal)))
        do (set-index-group traversal path)))

(defmethod find-definitions-generic ((traversal traversal-kotlin) range)
  (let* ((q (make-queue))
         (src-path (cdr (assoc :path range)))
         (path (cdr (assoc :path range)))
         (start-offset (cdr (assoc :start-offset range)))
         (end-offset (cdr (assoc :end-offset range)))
         (root-ast (get-ast (traversal-index traversal) path))
         ast
         results)
    (enqueue q root-ast)
    (loop
      do
      (setf ast (dequeue q))
      (if (null ast) (return))

      (when (and
              (equal (ast-value ast "type") "FUN")
              (contains-offset (jsown:val (jsown:val ast "textRange") "startOffset")
                               (jsown:val (jsown:val ast "textRange") "endOffset")            
                               start-offset
                               end-offset))
        (push
          (let ((pos (list
                       (cons :path src-path)
                       (cons :name (jsown:val ast "name"))
                       (cons :fq-name (find-fq-name-for-definition ast root-ast
                                                                   path (traversal-index traversal)))
                       (cons :top-offset (jsown:val (jsown:val ast "textRange") "startOffset")))))
            (when (assoc :origin range)
              (push (cons :origin (cdr (assoc :origin range))) pos))
            pos)
          results))

      (loop for child in (jsown:val ast "children") do (enqueue q child)))
    results))

(defun find-fq-name-for-definition (target-ast root-ast path index)
  (loop
    with stack = (list root-ast)
    with ast
    with result
    do
    (setf ast (pop stack))
    (unless ast (return result))
    
    (when (equal (ast-value ast "type") "PACKAGE_DIRECTIVE")
      (setf result (format nil "~{~a~^.~}" (get-dot-expressions (first (ast-value ast "children"))))))
    (when (equal (ast-value ast "type") "CLASS")
      (setf result (concatenate 'string result "." (ast-value ast "name"))))
    (when (and (eq ast target-ast)
               (equal (ast-value ast "type") "FUN"))
      (setf result (concatenate 'string result "." (ast-value ast "name"))) 
      (loop for param in (trav:get-asts ast '("VALUE_PARAMETER_LIST" "VALUE_PARAMETER" "TYPE_REFERENCE"))
            do
            (setf result (concatenate
                           'string
                           result
                           "-"
                           (find-fq-class-name-kotlin param path index)))))

    (loop for child in (ast-value ast "children")
          do (setf stack (append stack (list child))))))

(defmethod find-reference ((traversal traversal-kotlin) target-pos fq-name ast path)
  (when (equal fq-name (cdr (assoc :fq-name target-pos)))
    (list
      (cons :path path)
      (cons :top-offset (ast-value ast "textOffset")))))

(defmethod find-fq-name-generic ((traversal traversal-kotlin) ast path)
  (find-fq-name-for-reference ast path (traversal-index traversal)))

(defun find-fq-name-for-reference (ast path index)
  (alexandria:switch ((ast-value ast "type") :test #'equal)
    ("CALL_EXPRESSION"
     (let ((root (first (trav:get-asts ast '("DOT_QUALIFIED_EXPRESSION") :direction :upward))))
       (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
               (if (trav:get-asts root '("DOT_QUALIFIED_EXPRESSION"))
                   (format nil "~{~a~^.~}"
                           (append
                             (get-dot-expressions (first (ast-value root "children")))
                             (list
                               (ast-value (first (trav:get-asts root '("DOT_QUALIFIED_EXPRESSION"
                                                                       "CALL_EXPRESSION"
                                                                       "REFERENCE_EXPRESSION")))
                                          "name"))))
                   (find-fq-class-name-kotlin ast path index))
               (ast-value (first (trav:get-asts ast '("REFERENCE_EXPRESSION"))) "name")
               (mapcar (lambda (arg) (find-fq-class-name-kotlin arg path index))
                       (or (trav:get-asts ast '("VALUE_ARGUMENT_LIST" "VALUE_ARGUMENT" "*"))
                           (trav:get-asts ast '("LAMBDA_ARGUMENT" "LAMBDA_EXPRESSION" "*")))))))))

(defmethod find-fq-class-name-generic ((traversal traversal-kotlin) ast path)
  (find-fq-class-name-kotlin ast path (traversal-index traversal)))

(defun find-fq-class-name-kotlin (ast path index)
  (alexandria:switch ((ast-value ast "type") :test #'equal)
    ("NULL"
     "NULL")
    ("STRING_TEMPLATE"
     "java.lang.String")
    ("TYPE_REFERENCE"
     (let ((class-name (ast-value
                         (first (trav:get-asts ast '("USER_TYPE" "REFERENCE_EXPRESSION")))
                         "name")))
       (cond
         ((find class-name '("String") :test 'equal)
          (concatenate 'string "java.lang." class-name))
         ((equal class-name "Int")
          "INT"))))
    ("REFERENCE_EXPRESSION"
     (or (find-fq-class-name-by-variable-name (ast-value ast "name") ast path index)
         (find-fq-class-name-by-class-name (ast-value ast "name") ast)))
    ("DOT_QUALIFIED_EXPRESSION"
     (cond
       ((trav:get-asts ast '("CLASS_LITERAL_EXPRESSION"))
        "java.lang.Class")
       ;; change to self call?
       ((trav:get-asts ast '("REFERENCE_EXPRESSION"))
        (find-fq-class-name-by-class-name
          (ast-value (first (trav:get-asts ast '("REFERENCE_EXPRESSION"))) "name")
          ast))))
    ("CALL_EXPRESSION"
     (when (trav:get-asts ast '("REFERENCE_EXPRESSION"))
       (let* ((name (trav:ast-value
                      (first (trav:get-asts ast '("REFERENCE_EXPRESSION")))
                      "name"))
              (std-sig (find-signature-for-stdlib name path))
              (fqcn (when std-sig (jsown:val std-sig "fqcn"))))
         (if fqcn
             fqcn
             (let ((parent (first (trav:get-asts ast
                                                 '("DOT_QUALIFIED_EXPRESSION")
                                                 :direction :upward))))
                 (if parent
                     (find-fq-class-name-by-variable-name
                       (ast-value (first (trav:get-asts parent '("REFERENCE_EXPRESSION"))) "name")
                       ast path index)
                     (find-fq-class-name-by-class-name
                       (ast-value (first (trav:get-asts ast '("REFERENCE_EXPRESSION"))) "name")
                       ast)))))))
    (t
      (ast-value ast "type"))))

(defun find-fq-class-name-by-class-name (class-name ast)
  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (unless ast (return))

    (when (equal (ast-value ast "type") "kotlin.FILE")
      (let ((import (first (ast-find-suffix
                             (trav:get-asts ast '("IMPORT_LIST" "IMPORT_DIRECTIVE"))
                             (concatenate 'string "." class-name)
                             :key-name "fqName"))))
        (return (if import
                    (ast-value import "fqName")
                    (format nil "~{~a~^.~}"
                            (append (mapcar (lambda (ast) (ast-value ast "name"))
                                            (trav:get-asts ast '("PACKAGE_DIRECTIVE"
                                                                 "DOT_QUALIFIED_EXPRESSION"
                                                                 "REFERENCE_EXPRESSION")))
                                    (list class-name)))))))

    (enqueue q (ast-value ast "parent"))))

(defun find-fq-class-name-by-variable-name (variable-name ast path index)
  (unless variable-name
    (return-from find-fq-class-name-by-variable-name))

  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (unless ast (return))

    (when (equal (ast-value ast "type") "CLASS")
      (let* ((v (first (trav:filter-by-name (trav:get-asts ast '("PRIMARY_CONSTRUCTOR"
                                                                 "VALUE_PARAMETER_LIST"
                                                                 "VALUE_PARAMETER"))
                                            variable-name)))
             (fq-name (when v (find-fq-class-name-by-class-name
                                (ast-value (first (trav:get-asts v '("TYPE_REFERENCE"
                                                                     "USER_TYPE"
                                                                     "REFERENCE_EXPRESSION")))
                                           "name")
                                ast))))
        (when fq-name
          (return-from find-fq-class-name-by-variable-name fq-name))))

    (when (equal (ast-value ast "type") "FUN")
      (let* ((v (first (trav:filter-by-name (trav:get-asts ast '("VALUE_PARAMETER_LIST"
                                                                 "VALUE_PARAMETER"))
                                            variable-name)))
             (fq-name (when v (find-fq-class-name-by-class-name
                                (ast-value (first (or (trav:get-asts v '("TYPE_REFERENCE"
                                                                         "USER_TYPE"
                                                                         "REFERENCE_EXPRESSION"))
                                                      (trav:get-asts v '("TYPE_REFERENCE"
                                                                         "NULLABLE_TYPE"
                                                                         "USER_TYPE"
                                                                         "REFERENCE_EXPRESSION"))))
                                           "name")
                                ast))))
        (when fq-name
          (return-from find-fq-class-name-by-variable-name fq-name))))

    (when (and (equal variable-name "it")
               (equal (ast-value ast "type") "LAMBDA_EXPRESSION"))
      (let* ((root (first (trav:get-asts ast '("LAMBDA_ARGUMENT"
                                               "CALL_EXPRESSION"
                                               "DOT_QUALIFIED_EXPRESSION")
                                         :direction :upward)))
             (fq-name (or (find-fq-name-for-reference
                            (let* ((vn (trav:ast-value
                                         (first (trav:get-asts root '("REFERENCE_EXPRESSION")))
                                         "name"))
                                   (v (find-variable vn root)))
                              (first (trav:get-asts v '("CALL_EXPRESSION"))))
                            path index)
                          (find-fq-name-for-reference
                            (first (trav:get-asts root '("CALL_EXPRESSION")))
                            path index)))
             (split-fq-names (split #\- fq-name)))
        ;; get first type of arguments
        (return (second split-fq-names))))

    (enqueue q (ast-value ast "parent"))))

(defun find-variable (variable-name ast)
  (unless variable-name
    (return-from find-variable))

  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (unless ast (return))

    (let ((variable (first (trav:filter-by-name
                             (trav:get-asts ast '("PROPERTY") :direction :horizontal)
                             variable-name))))
      (return variable))

    (enqueue q (ast-value ast "parent"))))

(defun get-dot-expressions (ast)
  (cond
    ((equal (ast-value ast "type") "REFERENCE_EXPRESSION")
     (list (ast-value ast "name")))
    ((equal (ast-value ast "type") "DOT_QUALIFIED_EXPRESSION")
     (let ((results))
       (labels ((get-names (ast)
                  (loop for child in (trav:get-asts ast '("REFERENCE_EXPRESSION"))
                        with names
                        do
                        (setf names (append names (get-names child)))
                        (setf names (append names (list (ast-value child "name"))))
                        finally (return names))))
         (setf results (append (get-names ast) results))) results))))

(defmethod find-class-hierarchy-generic ((traversal traversal-kotlin)
                                         fq-class-name root-ast path index)
  (loop
    with stack = (list root-ast)
    with ast
    with target-package-name = (format nil "~{~a~^.~}" (butlast (split #\. fq-class-name)))
    with target-class-name = (first (last (split #\. fq-class-name)))
    initially
    (let ((hierarchy (load-hierarchy fq-class-name path)))
      (when hierarchy
        (return-from find-class-hierarchy-generic hierarchy)))
    do
    (setf ast (pop stack))
    (if (null ast) (return))

    (when (equal (ast-value ast "type") "kotlin.FILE")
      (unless (equal (format nil "~{~a~^.~}"
                             (mapcar (lambda (ast) (ast-value ast "name"))
                                     (trav:get-asts ast '("PACKAGE_DIRECTIVE"
                                                          "DOT_QUALIFIED_EXPRESSION"
                                                          "REFERENCE_EXPRESSION"))))
                     target-package-name)
        (return-from find-class-hierarchy-generic)))
    (when (equal (ast-value ast "type") "CLASS")
      (unless (equal (ast-value ast "name") target-class-name)
        (return-from find-class-hierarchy-generic))
      (return-from find-class-hierarchy-generic
        ;; TODO: fix parent class get
        (let ((parent-class-name (ast-value (first (trav:get-asts ast '("IDENTIFIER"))) "name")))
          (if parent-class-name
              (let ((parent-fq-class-name (find-fq-class-name-by-class-name parent-class-name ast)))
                (when parent-fq-class-name
                  (append (find-class-hierarchy parent-fq-class-name index)
                          (list parent-fq-class-name)
                          (list fq-class-name))))
              '("java.lang.Object")))))
    (loop for child in (jsown:val ast "children")
          do (setf stack (append stack (list child))))))

(defun find-signature-for-stdlib (target-name path)
  ;; FIXME: support for all stdlib classes
  ;; https://kotlinlang.org/api/latest/jvm/stdlib/
  (loop for fqcn in '("kotlin.collections.CollectionsKt")
        do
        (let ((found-method (find-if (lambda (method)
                                       (equal (jsown:val method "name") target-name))
                                     (load-signatures fqcn path))))
          (when found-method
            (push `("fqcn" . ,fqcn) (cdr found-method))
            (return found-method)))))

