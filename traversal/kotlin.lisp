(defpackage #:inga/traversal/kotlin
  (:use #:cl
        #:inga/traversal/base
        #:inga/utils)
  (:import-from #:inga/ast-index
                #:ast-index-paths
                #:ast-index-root-path
                #:create-indexes
                #:get-ast
                #:stop-indexes)
  (:import-from #:inga/file
                #:get-file-type)
  (:import-from #:inga/logger
                #:log-error)
  (:import-from #:inga/plugin/jvm-dependency-loader
                #:load-hierarchy 
                #:load-signatures
                #:load-structure)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path)
  (:export #:traversal-kotlin))
(in-package #:inga/traversal/kotlin)

(defclass traversal-kotlin (traversal)
  ())

(defmethod start-traversal ((kind (eql :kotlin)) include exclude path index)
  (setf *traversals*
        (acons :kotlin
               (make-instance 'traversal-kotlin
                              :path path
                              :index index)
               *traversals*))
  (create-indexes index :java include '("*.kt") exclude)
  (create-index-groups (cdr (assoc :kotlin *traversals*)))
  (cdr (assoc :kotlin *traversals*)))

(defmethod stop-traversal ((traversal traversal-kotlin))
  (stop-indexes (traversal-index traversal))
  (setf *traversals* nil))

(defun create-index-groups (traversal)
  (loop for path in (remove-if-not (lambda (p) (eq (get-file-type p) :kotlin))
                                   (ast-index-paths (traversal-index traversal)))
        do (set-index-group traversal path)))

(defmethod set-index-group ((trav traversal-kotlin) path)
  (let ((index-key (find-package-index-key (get-ast (traversal-index trav) path))))
    (unless (gethash :package *file-index*)
      (setf (gethash :package *file-index*) (make-hash-table)))
    (push path (gethash index-key (gethash :package *file-index*))))
  (let ((index-key (find-project-index-key
                      (merge-pathnames path (ast-index-root-path (traversal-index trav))))))
    (unless (gethash :module *file-index*)
      (setf (gethash :module *file-index*) (make-hash-table)))
    (push path (gethash index-key (gethash :module *file-index*)))))

(defun find-package-index-key (ast)
  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (unless ast (return))

    (when (equal (ast-value ast "type") "kotlin.FILE")
      (return-from find-package-index-key
                   (intern (format nil "~{~a~^.~}"
                                   (get-dot-expressions
                                     (first (get-asts ast '("PACKAGE_DIRECTIVE" "*")))))
                           :keyword)))

    (loop for child in (jsown:val ast "children") do (enqueue q child))))

(defun find-project-index-key (path)
  (let ((base-path (find-base-path path)))
    (when base-path (intern (namestring base-path) :keyword))))

(defmethod get-scoped-index-paths-generic ((trav traversal-kotlin) pos)
  (cond
    ((eq (cdr (assoc :type pos)) :module-private)
     (list (cdr (assoc :path pos))))
    ((eq (cdr (assoc :type pos)) :module-default)
     (ast-index-paths (traversal-index trav)))
    (t
     (log-error (format nil "unexpected visibility modifiers. type: ~a" (cdr (assoc :type pos))))
     (ast-index-paths (traversal-index trav)))))

(defmethod find-definitions-generic ((traversal traversal-kotlin) range)
  (let* ((q (make-queue))
         (src-path (cdr (assoc :path range)))
         (path (cdr (assoc :path range)))
         (start-offset (cdr (assoc :start-offset range)))
         (end-offset (cdr (assoc :end-offset range)))
         (ast (get-ast (traversal-index traversal) path)) 
         results)
    (enqueue q ast)
    (loop
      do
      (setf ast (dequeue q))
      (unless ast (return))

      (when (and (or (equal(ast-value ast "type") "FUN")
                     (equal (ast-value ast "type") "PRIMARY_CONSTRUCTOR"))
                 (contains-offset (jsown:val (jsown:val ast "textRange") "startOffset")
                                  (jsown:val (jsown:val ast "textRange") "endOffset")            
                                  start-offset
                                  end-offset))
        (let ((method-name (ast-value
                             (if (equal (ast-value ast "type") "PRIMARY_CONSTRUCTOR")
                                 (first (get-asts ast '("CLASS") :direction :upward))
                                 ast)
                             "name")))
          (push
            (let ((pos (list
                         (cons :type (get-scope ast))
                         (cons :path src-path)
                         (cons :name method-name)
                         (cons :fq-name (find-fq-name-for-definition
                                          method-name ast path (traversal-index traversal)))
                         (cons :top-offset (jsown:val (jsown:val ast "textRange") "startOffset")))))
              (when (assoc :origin range)
                (push (cons :origin (cdr (assoc :origin range))) pos))
              pos)
            results)))
      (when (equal (ast-value ast "type") "CLASS")
        (loop for inner-class in (get-asts ast '("CLASS_BODY" "CLASS"))
              do (enqueue q inner-class)))

      (loop for child in (jsown:val ast "children") do (enqueue q child)))
    (remove-duplicates results :test #'equal)))

(defun get-scope (ast)
  (let ((visibility (ast-value (first (get-asts ast '("MODIFIER_LIST" "*"))) "type")))
    (cond
      ((equal visibility "private") :module-private)
      (t :module-default))))

(defun find-fq-name-for-definition (fun-name ast path index)
  (loop
    with q = (make-queue)
    with fq-class-names
    with inner-class-names
    with method
    with fq-param-names
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (unless ast
      (pop inner-class-names)
      (return (format nil "~{~a~^.~}~{$~a~^$~}.~a~:[~;-~]~:*~{~a~^-~}"
                      fq-class-names inner-class-names method fq-param-names)))
    
    (when (equal (ast-value ast "type") "kotlin.FILE")
      (setf fq-class-names
            (append (get-dot-expressions (first (get-asts ast '("PACKAGE_DIRECTIVE" "*"))))
                    fq-class-names)))

    (when (and (equal (ast-value ast "type") "FUN") (equal (ast-value ast "name") fun-name))
      (setf method fun-name)
      (loop for param in (get-asts ast '("VALUE_PARAMETER_LIST"
                                         "VALUE_PARAMETER"
                                         "TYPE_REFERENCE"))
            do (setf fq-param-names (append (list (find-fq-class-name-kotlin param path index))
                                            fq-param-names))))

    (when (equal (ast-value ast "type") "CLASS")
      (when inner-class-names
        (setf fq-class-names (butlast fq-class-names)))
      (setf fq-class-names (append (list (ast-value ast "name")) fq-class-names))
      (setf inner-class-names (append (list (ast-value ast "name")) inner-class-names))

      ;; if constructor
      (when (and (not method) (equal (ast-value ast "name") fun-name))
        (setf method (ast-value ast "name"))
        (loop for param in (get-asts ast '("PRIMARY_CONSTRUCTOR"
                                           "VALUE_PARAMETER_LIST"
                                           "VALUE_PARAMETER"))
              do (setf fq-param-names (append (list (find-fq-class-name-kotlin param path index))
                                              fq-param-names))))
      
      (when (and (not method) (filter-by-name (get-asts ast '("CLASS_BODY" "FUN")) fun-name))
        (setf method fun-name)
        (loop for param in (get-asts ast '("CLASS_BODY"
                                           "FUN"
                                           "VALUE_PARAMETER_LIST"
                                           "VALUE_PARAMETER"
                                           "TYPE_REFERENCE"))
              do (setf fq-param-names (append (list (find-fq-class-name-kotlin param path index))
                                              fq-param-names)))))

    (enqueue q (ast-value ast "parent"))))

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
     (cond
       ((is-member (ast-value (first (get-asts ast '("REFERENCE_EXPRESSION"))) "name") ast)
        (find-fq-name-for-definition (ast-value
                                       (first (get-asts ast '("REFERENCE_EXPRESSION")))
                                       "name")
                                     ast path index))
       (t
        (let ((root (first (get-asts ast '("DOT_QUALIFIED_EXPRESSION") :direction :upward))))
          (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
                  (if (get-asts root '("DOT_QUALIFIED_EXPRESSION"))
                      (let ((v (first (get-asts root '("PROPERTY") :direction :horizontal))))
                        (if v
                            (find-fq-class-name-kotlin v path index)
                            (let* ((fq-name (find-fq-name-for-reference
                                              (first (get-asts root '("DOT_QUALIFIED_EXPRESSION"
                                                                      "CALL_EXPRESSION")))
                                              path index))
                                   (method (find-signature
                                             fq-name
                                             #'(lambda (fqcn) (load-signatures fqcn path))
                                             index)))
                              (if method
                                  (cdr (assoc :return method))
                                  (format nil "~{~a~^.~}"
                                          (append
                                            (get-dot-expressions (first (ast-value root "children")))
                                            (list
                                              (ast-value (first (get-asts root '("DOT_QUALIFIED_EXPRESSION"
                                                                                 "CALL_EXPRESSION"
                                                                                 "REFERENCE_EXPRESSION")))
                                                         "name"))))))))
                      (find-fq-class-name-kotlin ast path index))
                  (ast-value (first (get-asts ast '("REFERENCE_EXPRESSION"))) "name")
                  (mapcar (lambda (arg) (find-fq-class-name-kotlin arg path index))
                          (or (get-asts ast '("VALUE_ARGUMENT_LIST" "VALUE_ARGUMENT" "*"))
                              (get-asts ast '("LAMBDA_ARGUMENT" "LAMBDA_EXPRESSION" "*")))))))))))

(defun is-member (ref-name ast)
  (when (or (get-asts ast '("DOT_QUALIFIED_EXPRESSION") :direction :upward)
            (get-asts ast '("SAFE_ACCESS_EXPRESSION") :direction :upward))
    (return-from is-member))

  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (unless ast (return))
    
    (when (and (equal (ast-value ast "type") "CLASS")
               (filter-by-name (get-asts ast '("CLASS_BODY" "FUN")) ref-name)
               (return t)))

    (enqueue q (ast-value ast "parent"))))

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
                         (first (or (get-asts ast '("NULLABLE_TYPE"
                                                    "USER_TYPE"
                                                    "REFERENCE_EXPRESSION"))
                                    (get-asts ast '("USER_TYPE"
                                                    "REFERENCE_EXPRESSION"))))
                         "name")))
       (cond
         ((find class-name '("String") :test 'equal)
          (concatenate 'string "java.lang." class-name))
         ((equal class-name "Int")
          "INT")
         (t
          (find-fq-class-name-by-class-name class-name ast path index)))))
    ("OBJECT_LITERAL"
     (find-fq-class-name-kotlin
       (first (or (get-asts ast '("OBJECT_DECLARATION"
                                  "SUPER_TYPE_LIST"
                                  "SUPER_TYPE_ENTRY"
                                  "TYPE_REFERENCE"))
                  (get-asts ast '("OBJECT_DECLARATION"
                                  "SUPER_TYPE_LIST"
                                  "SUPER_TYPE_CALL_ENTRY"
                                  "CONSTRUCTOR_CALLEE"
                                  "TYPE_REFERENCE"))))
       path index))
    ("PROPERTY"
     (or
       ;; if create an instance of a class
       (find-fq-class-name-kotlin (first (get-asts ast '("DOT_QUALIFIED_EXPRESSION"
                                                         "CALL_EXPRESSION")))
                                  path index)
       (find-fq-class-name-kotlin (first (get-asts ast '("CALL_EXPRESSION"))) path index)
       (find-fq-class-name-kotlin (first (get-asts ast '("TYPE_REFERENCE"))) path index)))
    ("VALUE_PARAMETER"
     (find-fq-class-name-kotlin (or (first (get-asts ast '("REFERENCE_EXPRESSION")))
                                    (first (get-asts ast '("TYPE_REFERENCE"))))
                                path index))
    ("REFERENCE_EXPRESSION"
     (or (find-fq-class-name-by-variable-name (ast-value ast "name") ast path index)
         (find-fq-class-name-by-class-name (ast-value ast "name") ast path index)))
    ("DOT_QUALIFIED_EXPRESSION"
     (cond
       ((get-asts ast '("CLASS_LITERAL_EXPRESSION"))
        "java.lang.Class")
       ;; change to self call?
       ((get-asts ast '("REFERENCE_EXPRESSION"))
        (find-fq-class-name-by-class-name
          (ast-value (first (get-asts ast '("REFERENCE_EXPRESSION"))) "name")
          ast path index))))
    ("CALL_EXPRESSION"
     (when (get-asts ast '("REFERENCE_EXPRESSION"))
       (let* ((name (ast-value
                      (first (get-asts ast '("REFERENCE_EXPRESSION")))
                      "name"))
              (fqcn (find-signature-for-stdlib name path)))
         (if fqcn
             fqcn
             (let ((parent (or (first (get-asts ast '("DOT_QUALIFIED_EXPRESSION") :direction :upward))
                               (first (get-asts ast '("SAFE_ACCESS_EXPRESSION") :direction :upward)))))
               (if parent
                   ;; if method chains
                   (if (get-asts parent '("DOT_QUALIFIED_EXPRESSION"))
                       (let* ((fq-name (find-fq-name-for-reference ast path index))
                              (method (find-signature fq-name
                                                      #'(lambda (fqcn) (load-signatures fqcn path))
                                                      index)))
                         (when method (cdr (assoc :return method))))
                       (if (find-definition 
                             (ast-value (first (get-asts parent '("REFERENCE_EXPRESSION"))) "name")
                             parent)
                           (find-fq-class-name-by-variable-name
                             (ast-value (first (get-asts parent '("REFERENCE_EXPRESSION"))) "name")
                             ast path index)
                           (or 
                             (find-fq-class-name-by-class-name
                                 (ast-value (first (get-asts parent '("REFERENCE_EXPRESSION"))) "name")
                                 ast path index)  
                             (find-fq-class-name-kotlin
                               (first (get-asts ast '("REFERENCE_EXPRESSION")))
                               path index))))
                   (find-fq-class-name-by-class-name
                     (ast-value (first (get-asts ast '("REFERENCE_EXPRESSION"))) "name")
                     ast path index)))))))
    (t
      (ast-value ast "type"))))

(defun find-fq-class-name-by-class-name (class-name ast path index)
  (unless class-name (return-from find-fq-class-name-by-class-name))

  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (unless ast (return))

    (when (equal (ast-value ast "type") "kotlin.FILE")
      (let ((import (first (ast-find-suffix
                             (get-asts ast '("IMPORT_LIST" "IMPORT_DIRECTIVE"))
                             (concatenate 'string "." class-name)
                             :key-name "fqName"))))
        (return (if import
                    (ast-value import "fqName")
                    (if (get-asts ast '("CLASS" "SUPER_TYPE_LIST"))
                        (loop for super in (get-asts ast '("CLASS"
                                                           "SUPER_TYPE_LIST"
                                                           "SUPER_TYPE_CALL_ENTRY"
                                                           "CONSTRUCTOR_CALLEE"
                                                           "TYPE_REFERENCE"
                                                           "USER_TYPE"
                                                           "REFERENCE_EXPRESSION"))
                              do
                              (return (if (equal (ast-value super "name") class-name)
                                          (format nil "~{~a~^.~}"
                                                  (append (get-dot-expressions
                                                            (first (get-asts
                                                                     ast
                                                                     '("PACKAGE_DIRECTIVE" "*"))))
                                                          (list class-name)))
                                          (let ((fq-class-name
                                                  (find-if (lambda (name)
                                                             (equal (first (last (split #\$ (jsown:val name "name"))))
                                                                    class-name))
                                                           (mapcan (lambda (fqcn)
                                                                     (loop for path in (gethash (intern 
                                                                                                  (get-package-name fqcn) :keyword)
                                                                                                (gethash :package *file-index*))
                                                                           do
                                                                           (let ((s (load-structure fqcn path)))
                                                                             (when s (return s)))))
                                                                   (find-class-hierarchy
                                                                     (find-fq-class-name-by-class-name
                                                                       (ast-value super "name")
                                                                       super path index)
                                                                     index)))))
                                            (when fq-class-name (jsown:val fq-class-name "name"))))))
                        (format nil "~{~a~^.~}"
                                (append (get-dot-expressions
                                          (first (get-asts ast '("PACKAGE_DIRECTIVE" "*"))))
                                        (list class-name))))))))
    (enqueue q (ast-value ast "parent"))))

(defun find-fq-class-name-by-variable-name (variable-name ast path index)
  (when variable-name
    (find-fq-class-name-kotlin (find-definition variable-name ast) path index)))

(defmethod find-reference-to-literal-generic ((trav traversal-kotlin) ast path)
  (cond
    ((get-asts ast '("LITERAL_STRING_TEMPLATE_ENTRY"))
     `(((:path . ,path)
        (:name .
         ,(ast-value (first (get-asts ast '("LITERAL_STRING_TEMPLATE_ENTRY"))) "name"))
        (:top-offset . ,(ast-value ast "textOffset")))))
    ((equal (ast-value ast "type") "STRING_TEMPLATE")
     (loop for entry in (get-asts ast '("LONG_STRING_TEMPLATE_ENTRY"))
       do
       (multiple-value-bind (v vi) (find-definition
                                     (ast-value
                                       (first (get-asts entry '("DOT_QUALIFIED_EXPRESSION"
                                                                "REFERENCE_EXPRESSION")))
                                       "name")
                                     entry)
         (let* ((defs
                  (when v (find-definitions
                            `((:path . ,path)
                              (:start-offset .
                               ,(ast-value (ast-value v "textRange") "startOffset"))
                              (:end-offset .
                               ,(ast-value (ast-value v "textRange") "endOffset"))))))
                (refs (mapcan (lambda (d) (find-references d (traversal-index trav)))
                              (remove-duplicates defs :test #'equal))))
           (when refs
             (return (mapcan (lambda (ref-pos)
                               (let ((ast (nth vi (get-asts (find-ast ref-pos (traversal-index trav))
                                                            '("CALL_EXPRESSION"
                                                              "VALUE_ARGUMENT_LIST"
                                                              "VALUE_ARGUMENT"
                                                              "STRING_TEMPLATE")))))
                                 (find-reference-to-literal-generic
                                   trav ast (cdr (assoc :path ref-pos)))))
                             refs)))))))
    ((equal (ast-value ast "type") "REFERENCE_EXPRESSION")
     (let ((def (find-definition (ast-value ast "name") ast)))
       (find-reference-to-literal-generic trav (first (get-asts def '("STRING_TEMPLATE"))) path)))))

(defmethod find-caller-generic ((trav traversal-kotlin) fq-names ast path)
  (labels ((find-caller (fq-names ast path)
             (unless ast (return-from find-caller))

             (let* ((found-fq-name (find-fq-name ast path))
                    (matched-api (find-signature found-fq-name
                                                 #'(lambda (fqcn) fq-names)
                                                 (traversal-index trav))))
               (if matched-api
                   (values ast matched-api)
                   (let* ((root (first (get-asts ast '("DOT_QUALIFIED_EXPRESSION")
                                                 :direction :upward)))
                          (callee (first (get-asts root '("DOT_QUALIFIED_EXPRESSION"
                                                          "CALL_EXPRESSION")))))
                     (if callee
                         (find-caller fq-names callee path)
                         (progn
                           (find-definition
                             (ast-value (first (get-asts ast '("REFERENCE_EXPRESSION")
                                                         :direction :horizontal))
                                        "name")
                             ast #'find-caller-by-visitor)
                           (loop for arg in (get-asts ast '("VALUE_ARGUMENT_LIST"
                                                            "VALUE_ARGUMENT"
                                                            "REFERENCE_EXPRESSION"))
                                 do
                                 (find-definition
                                   (ast-value arg "name")
                                   arg #'find-caller-by-visitor))
                           nil))))))
           (find-caller-by-visitor (ast)
             (multiple-value-bind (caller api) (find-caller
                                                 fq-names
                                                 (first (get-asts ast '("CALL_EXPRESSION")))
                                                 path)
               (when caller
                 (return-from find-caller-generic (values caller api))))
             (multiple-value-bind (caller api) (find-caller
                                                 fq-names
                                                 (first (get-asts ast '("DOT_QUALIFIED_EXPRESSION"
                                                                        "CALL_EXPRESSION")))
                                                 path)
               (when caller
                 (return-from find-caller-generic (values caller api))))))
    (find-caller fq-names ast path)))

(defun find-definition (variable-name ast &optional visitor)
  (unless variable-name
    (return-from find-definition))

  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (unless ast (return))
    (when visitor
      (loop for ast in (remove-if (lambda (sibling)
                                    (> (ast-value sibling "textOffset") (ast-value ast "textOffset")))
                                  (get-asts ast '("*") :direction :horizontal))
            do (funcall visitor ast)))

    (when (equal (ast-value ast "type") "CLASS")
      (loop for inner-class in (get-asts ast '("CLASS_BODY" "CLASS"))
        do (enqueue q inner-class))
      (loop for dec in (get-asts ast '("CLASS_BODY" "OBJECT_DECLARATION"))
        do (enqueue q dec))

      (loop for param in (get-asts ast '("PRIMARY_CONSTRUCTOR"
                                         "VALUE_PARAMETER_LIST"
                                         "VALUE_PARAMETER"))
            and index from 0
            do (when (equal (ast-value param "name") variable-name)
                 (return-from find-definition (values param index))))

      (let ((v (first (filter-by-name (get-asts ast '("CLASS_BODY" "PROPERTY"))
                                      variable-name))))
        (when v (return v))))

    (when (equal (ast-value ast "type") "OBJECT_DECLARATION")
      (let ((v (first (filter-by-name (get-asts ast '("CLASS_BODY" "PROPERTY"))
                                      variable-name))))
        (when v (return v))))

    (when (equal (ast-value ast "type") "FUN")
      (let ((v (first (filter-by-name (get-asts ast '("VALUE_PARAMETER_LIST"
                                                      "VALUE_PARAMETER"))
                                      variable-name))))
        (when v (return v))))

    (when (and (equal variable-name "it")
               (equal (ast-value ast "type") "LAMBDA_EXPRESSION"))
      (let ((root (first (get-asts ast '("LAMBDA_ARGUMENT"
                                         "CALL_EXPRESSION"
                                         "DOT_QUALIFIED_EXPRESSION")
                                   :direction :upward))))
        (return (if (get-asts root '("REFERENCE_EXPRESSION"))
                    (let ((vn (ast-value
                                (first (get-asts root '("REFERENCE_EXPRESSION")))
                                "name")))
                      (first (get-asts (find-definition vn root)
                                       '("CALL_EXPRESSION"
                                         "VALUE_ARGUMENT_LIST"
                                         "VALUE_ARGUMENT"
                                         "*"))))
                    (first (get-asts root
                                     '("CALL_EXPRESSION"
                                       "VALUE_ARGUMENT_LIST"
                                       "VALUE_ARGUMENT"
                                       "*")))))))

    (let ((v (first (filter-by-name
                      (get-asts ast '("PROPERTY") :direction :horizontal)
                      variable-name))))
      (when v (return v)))

    (enqueue q (ast-value ast "parent"))))

(defun get-dot-expressions (ast)
  (cond
    ((equal (ast-value ast "type") "REFERENCE_EXPRESSION")
     (list (ast-value ast "name")))
    ((equal (ast-value ast "type") "DOT_QUALIFIED_EXPRESSION")
     (let ((results))
       (labels ((get-names (ast)
                  (loop for child in (get-asts ast '("REFERENCE_EXPRESSION"))
                        with names
                        do
                        (setf names (append names (get-names child)))
                        (setf names (append names (list (ast-value child "name"))))
                        finally (return names))))
         (when (get-asts ast '("DOT_QUALIFIED_EXPRESSION"))
           (setf results (append (get-dot-expressions
                                   (first (get-asts ast '("DOT_QUALIFIED_EXPRESSION")))))))
         (setf results (append results (get-names ast))))
       results))))

(defmethod find-class-hierarchy-generic ((traversal traversal-kotlin)
                                         fq-class-name root-ast path index)
  (loop
    with stack = (list root-ast)
    with ast
    with target-package-name = (get-package-name fq-class-name)
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
                                     (get-asts ast '("PACKAGE_DIRECTIVE"
                                                     "DOT_QUALIFIED_EXPRESSION"
                                                     "REFERENCE_EXPRESSION"))))
                     target-package-name)
        (return-from find-class-hierarchy-generic)))

    (when (equal (ast-value ast "type") "CLASS")
      (unless (equal (ast-value ast "name") target-class-name)
        (return-from find-class-hierarchy-generic))
      (return-from find-class-hierarchy-generic
        ;; TODO: fix parent class get
        (let ((parent-class-name (ast-value (first (get-asts ast '("IDENTIFIER"))) "name")))
          (if parent-class-name
              (let ((parent-fq-class-name (find-fq-class-name-by-class-name parent-class-name
                                                                            ast path index)))
                (when parent-fq-class-name
                  (append (find-class-hierarchy parent-fq-class-name index)
                          (list parent-fq-class-name)
                          (list fq-class-name))))
              '("java.lang.Object")))))

    (loop for child in (jsown:val ast "children")
          do (setf stack (append stack (list child))))))

(defun get-package-name (fq-class-name)
  (format nil "~{~a~^.~}" (butlast (split #\. fq-class-name))))

(defun find-signature-for-stdlib (target-name path)
  ;; FIXME: support for all stdlib classes
  ;; https://kotlinlang.org/api/latest/jvm/stdlib/
  (loop for fqcn in '("kotlin.collections.CollectionsKt")
        do
        (let ((found-method (find-if (lambda (method)
                                       (equal (cdr (assoc :name method)) target-name))
                                     (load-signatures fqcn path))))
          (when found-method (return fqcn)))))

