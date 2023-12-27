(defpackage #:inga/traversal/java
  (:use #:cl
        #:inga/traversal/base
        #:inga/utils)
  (:import-from #:alexandria
                #:switch)
  (:import-from #:inga/ast-index
                #:ast-index-paths
                #:ast-index-root-path
                #:clean-indexes
                #:create-indexes
                #:get-ast)
  (:import-from #:inga/traversal/kotlin
                #:traversal-kotlin)
  (:import-from #:inga/file
                #:get-file-type)
  (:import-from #:inga/plugin/jvm-dependency-loader
                #:load-signatures
                #:load-hierarchy)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path
                #:is-primitive-type)
  (:export #:traversal-java))
(in-package #:inga/traversal/java)

(defvar *package-index-groups* nil)
(defvar *project-index-groups* nil)

(defparameter *include-java* '("*.java"))

(defclass traversal-java (traversal)
  ())

(defmethod start-traversal ((kind (eql :java)) include exclude path index)
  (setf *traversals*
        (acons :java
               (make-instance 'traversal-java
                              :path path
                              :index index)
               *traversals*))
  (create-indexes index include *include-java* exclude)
  (create-index-groups index)
  (cdr (assoc :java *traversals*)))

(defmethod stop-traversal ((traversal traversal-java))
  (clean-index-groups) 
  (clean-indexes (traversal-index traversal))
  (setf *traversals* nil))

(defun create-index-groups (index)
  (loop for path in (remove-if-not (lambda (p) (eq (get-file-type p) :java))
                                   (ast-index-paths index))
        do
        (let ((index-key (find-package-index-key (get-ast index path))))
          (setf *package-index-groups*
                (if (assoc index-key *package-index-groups*)
                    (acons index-key
                           (append (list path) (cdr (assoc index-key *package-index-groups*)))
                           *package-index-groups*)
                    (acons index-key (list path) *package-index-groups*))))
        (let ((index-key (find-project-index-key
                           (merge-pathnames path (ast-index-root-path index)))))
          (setf *project-index-groups*
                (if (assoc index-key *project-index-groups*)
                    (acons index-key
                           (append (list path) (cdr (assoc index-key *project-index-groups*)))
                           *project-index-groups*)
                    (acons index-key (list path) *project-index-groups*))))))

(defun clean-index-groups ()
  (setf *package-index-groups* nil)
  (setf *project-index-groups* nil))

(defmethod get-scoped-index-paths-generic ((traversal traversal-java) pos)
  (cond
    ((eq (cdr (assoc :type pos)) :module-private)
     (list (cdr (assoc :path pos))))
    ((eq (cdr (assoc :type pos)) :module-default)
     (cdr (assoc
            (find-package-index-key (get-ast (traversal-index traversal) (cdr (assoc :path pos))))
            *package-index-groups*)))
    ((eq (cdr (assoc :type pos)) :module-public)
     (cdr (assoc
            (find-project-index-key (merge-pathnames (cdr (assoc :path pos)) (traversal-path traversal)))
            *project-index-groups*)))
    (t
     (ast-index-paths (traversal-index traversal)))))

(defun find-package-index-key (ast)
  (loop
    with stack = (list ast)
    do
    (setf ast (pop stack))
    (if (null ast) (return))

    (when (equal (ast-value ast "type") "PACKAGE")
      (return-from find-package-index-key (intern (ast-value ast "packageName"))))

    (loop for child in (jsown:val ast "children")
          do (setf stack (append stack (list child))))))

(defun find-project-index-key (path)
  (let ((base-path (find-base-path path)))
    (when base-path (intern (namestring base-path)))))

(defmethod find-definitions-generic ((traversal traversal-java) range)
  (let ((q (make-queue))
        (src-path (cdr (assoc :path range)))
        (path (cdr (assoc :path range)))
        (start-offset (cdr (assoc :start-offset range)))
        (end-offset (cdr (assoc :end-offset range)))
        ast
        root-ast
        results)
    (setf ast (get-ast (traversal-index traversal) path))
    (setf root-ast ast)
    (enqueue q ast)
    (loop
      do
      (setf ast (dequeue q))
      (if (null ast) (return))

      (when (and
              (or
                ;; field reference
                (and
                  (jsown:keyp ast "parent")
                  (equal (ast-value (jsown:val ast "parent") "type") "CLASS") 
                  (equal (ast-value ast "type") "VARIABLE"))
                  (equal (ast-value ast "type") "METHOD"))
              (contains-offset (jsown:val ast "startPos") (jsown:val ast "endPos")
                               start-offset end-offset)
              (jsown:keyp ast "name"))
        (let ((pos (list
                     (cons :type (get-scope ast))
                     (cons :path src-path)
                     (cons :name (jsown:val ast "name"))
                     (cons :fq-name (get-fq-name-of-declaration
                                      root-ast
                                      (jsown:val ast "pos")))
                     (cons :top-offset (jsown:val ast "pos")))))
          (when (assoc :origin range)
            (push (cons :origin (cdr (assoc :origin range))) pos))
          (setf results (append results (list pos)))))

      (loop for child in (jsown:val ast "children") do (enqueue q child)))
    results))

(defun get-scope (ast)
  (let ((modifiers (split-trim-comma (ast-value (first (trav:get-asts ast '("MODIFIERS"))) "name"))))
    (cond
      ((find "PUBLIC" modifiers :test #'equal) :module-public)
      ((find "PROTECTED" modifiers :test #'equal) :module-protected)
      ((find "PRIVATE" modifiers :test #'equal) :module-private)
      (t :module-default))))

(defmethod find-reference ((traversal traversal-java) target-pos fq-name ast path)
  (when (matches-signature fq-name (cdr (assoc :fq-name target-pos))
                           (traversal-index traversal))
    `((:path . ,path)
      (:top-offset . ,(ast-value ast "startPos")))))

(defmethod find-fq-name ((traversal traversal-java) ast path)
  (find-fq-name-for-reference ast path (traversal-index traversal)))

(defun find-fq-name-for-reference (ast path index)
  (alexandria:switch ((ast-value ast "type") :test #'equal)
    ("NEW_CLASS"
     (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
             (find-fq-class-name-by-class-name (ast-value ast "name") ast)
             (ast-value ast "name")
             (mapcar (lambda (arg) (find-fq-class-name-java arg path index))
                       (trav:get-asts ast '("*")))))
    ("METHOD_INVOCATION"
     (if (trav:get-asts ast '("MEMBER_SELECT"))
         (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
                 ;; if method chain
                 (cond
                   ((trav:get-asts ast '("MEMBER_SELECT" "METHOD_INVOCATION"))
                    (let ((fq-name (find-fq-name-for-reference
                                     (first (trav:get-asts ast '("MEMBER_SELECT" "METHOD_INVOCATION")))
                                     path index)))
                      (when fq-name
                        (let ((method (find-signature
                                        fq-name
                                        #'(lambda (fq-class-name)
                                            (load-signatures fq-class-name path))
                                        index)))
                          (when method
                            (jsown:val (jsown:val method "returnType") "name"))))))
                   ((trav:get-asts ast '("MEMBER_SELECT" "IDENTIFIER"))
                    (or (find-fq-class-name-by-variable-name
                          (ast-value (first (trav:get-asts ast '("MEMBER_SELECT" "IDENTIFIER"))) "name")
                          ast path index)
                        (find-fq-class-name-by-class-name
                          ;; IDENTIFIER is class name
                          (ast-value (first (trav:get-asts ast '("MEMBER_SELECT" "IDENTIFIER"))) "name")
                          ast)))
                   ((trav:get-asts ast '("MEMBER_SELECT" "NEW_CLASS"))
                    (find-fq-class-name-by-class-name
                      (ast-value (first (trav:get-asts ast '("MEMBER_SELECT" "NEW_CLASS"))) "name")
                      ast))
                   ((trav:get-asts ast '("MEMBER_SELECT" "MEMBER_SELECT"))
                    (find-fq-class-name-by-class-name
                      (ast-value (first (trav:get-asts ast '("MEMBER_SELECT" "MEMBER_SELECT"))) "name")
                      ast)))
                 (ast-value (first (trav:get-asts ast '("*"))) "name")
                 (mapcar (lambda (arg) (find-fq-class-name-java arg path index))
                       (nthcdr 1 (trav:get-asts ast '("*")))))
         (format nil "~a~:[~;-~]~:*~{~a~^-~}"
                 (find-fq-name-for-definition
                   (ast-value (first (trav:get-asts ast '("IDENTIFIER"))) "name")
                   ast)
                 (mapcar (lambda (arg) (find-fq-class-name-java arg path index))
                         (nthcdr 1 (trav:get-asts ast '("*")))))))))

(defmethod find-fq-class-name-generic ((traversal traversal-java) ast path)
  (find-fq-class-name-java ast path (traversal-index traversal)))

(defun find-fq-class-name-java (ast path index)
  (if (uiop:string-suffix-p (ast-value ast "type") "_LITERAL")
      (if (equal (ast-value ast "type") "STRING_LITERAL")
          "java.lang.String"
          (ppcre:regex-replace-all "_LITERAL" (ast-value ast "type") ""))
      (switch ((ast-value ast "type") :test #'equal)
        ("MEMBER_SELECT"
         (if (trav:filter-by-name (list ast) "class")
             "java.lang.Class"
             (find-fq-class-name-by-class-name
               (ast-value (first (trav:get-asts ast '("IDENTIFIER"))) "name") ast)))
        ("NEW_CLASS"
         (find-fq-class-name-by-class-name (ast-value ast "name") ast))
        ("VARIABLE"
         (find-fq-class-name-by-variable-name (ast-value ast "name") ast path index))
        ("IDENTIFIER"
         (find-fq-class-name-by-variable-name (ast-value ast "name") ast path index))
        ("METHOD_INVOCATION"
         (let ((fq-name (find-fq-name-for-reference ast path index)))
           (let ((method (find-signature
                           fq-name
                           #'(lambda (fq-class-name)
                               (load-signatures fq-class-name path))
                           index)))
             (when method (jsown:val (jsown:val method "returnType") "name"))))))))

(defun find-fq-class-name-by-class-name (class-name ast)
  (unless class-name
    (return-from find-fq-class-name-by-class-name))

  (cond
    ((is-primitive-type class-name)
     class-name)
    ((find class-name '("Long" "String") :test 'equal)
     (concatenate 'string "java.lang." class-name))
    ((equal class-name "class")
     "java.lang.Class")
    (t
     (loop
       with q = (make-queue)
       with fq-names
       initially (enqueue q ast)
       do
       (setf ast (dequeue q))
       (when (null ast) (return (format nil "~{~a~^.~}" fq-names)))

       (when (equal (ast-value ast "type") "COMPILATION_UNIT")
         (let ((import (first (ast-find-suffix
                                (trav:get-asts ast '("IMPORT"))
                                (concatenate 'string "." class-name)
                                :key-name "fqName"))))
           (setf fq-names
                 (if import
                     (list (ast-value import "fqName"))
                     (append
                       (list (ast-value (first (trav:get-asts ast '("PACKAGE"))) "packageName"))
                       fq-names)))))

       (when (equal (ast-value ast "type") "CLASS")
         ;; for inner class
         (if (or (trav:filter-by-name (trav:get-asts ast '("CLASS")) class-name)
                 (trav:filter-by-name (trav:get-asts ast '("ENUM")) class-name))
             (push (concatenate 'string (ast-value ast "name") "$" class-name) fq-names)
             (push class-name fq-names)))

       (when (jsown:keyp ast "parent")
         (enqueue q (jsown:val ast "parent")))))))

(defun find-fq-class-name-by-variable-name (variable-name ast path index)
  (let ((variable (find-variable variable-name ast)))
    (cond
      ((trav:get-asts variable '("PRIMITIVE_TYPE"))
       (find-fq-class-name-by-class-name
         (ast-value (first (trav:get-asts variable '("PRIMITIVE_TYPE"))) "name") variable))
      ((trav:get-asts variable '("IDENTIFIER"))
       (find-fq-class-name-by-class-name
         (ast-value (first (trav:get-asts variable '("IDENTIFIER"))) "name") variable))
      ((trav:get-asts variable '("PARAMETERIZED_TYPE"))
       (find-fq-class-name-by-class-name
         (ast-value (first (trav:get-asts variable '("PARAMETERIZED_TYPE"))) "name") variable))
      ((trav:get-asts variable '("METHOD_INVOCATION"))
       (let ((fq-name (find-fq-name-for-reference
                        (first (trav:get-asts variable '("METHOD_INVOCATION"))) path index)))
         (let ((method (find-signature
                         fq-name
                         #'(lambda (fq-class-name) (load-signatures fq-class-name path))
                         index)))
           (when method
             (jsown:val (jsown:val method "returnType") "name"))))))))

(defun find-variable (variable-name ast)
  (unless variable-name
    (return-from find-variable))

  (loop
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (when (null ast) (return))

    (let ((variable (first (if (equal (ast-value ast "type") "VARIABLE")
                               (trav:filter-by-name (list ast) variable-name)
                               (trav:filter-by-name (trav:get-asts ast '("VARIABLE")) variable-name)))))
      (when variable
        (return variable)))

    (enqueue q (ast-value ast "parent"))))

(defun find-fq-name-for-definition (method-name ast)
  (loop
    with q = (make-queue)
    with fq-names
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (when (null ast) (return (format nil "~{~a~^.~}" fq-names)))

    (when (trav:filter-by-name (trav:get-asts ast '("METHOD")) method-name)
      (setf fq-names (append fq-names (list method-name))))
    (when (and
            fq-names
            (equal (ast-value ast "type") "CLASS"))
      (setf fq-names (append (list (ast-value ast "name")) fq-names)))
    (when (and
            fq-names
            (equal (ast-value ast "type") "COMPILATION_UNIT"))
      (setf fq-names (append 
                       (list (ast-value (first (trav:get-asts ast '("PACKAGE"))) "packageName"))
                       fq-names)))

    (when (jsown:keyp ast "parent")
      (enqueue q (jsown:val ast "parent")))))

(defun get-fq-name-of-declaration (root-ast top-offset)
  (let ((stack (list root-ast))
        result)
    (loop
      with class-name
      do
      (let ((ast (pop stack)))
        (if (null ast) (return))

        (when (equal (ast-value ast "type") "PACKAGE")
          (setf result (format nil "~{~a~^.~}" (remove nil (list result (jsown:val ast "packageName"))))))
        (when (or
                (equal (ast-value ast "type") "CLASS")
                (equal (ast-value ast "type") "INTERFACE"))
          (setf class-name (jsown:val ast "name"))
          (setf result (format nil "~{~a~^.~}" (remove nil (list result (jsown:val ast "name"))))))
        (when (and
                (jsown:keyp ast "name")
                (= (jsown:val ast "pos") top-offset))
          (setf result (format nil "~{~a~^.~}"
                               (remove nil (list result (jsown:val ast "name")))))
          (when (equal (ast-value ast "type") "METHOD")
            (loop for child in (jsown:val ast "children")
                  do
                  (when (equal (jsown:val child "type") "VARIABLE")
                    (loop for child in (jsown:val child "children")
                          do
                          (when (and (ast-value child "name")
                                     (not (equal (ast-value child "name") "")))
                            (setf result (concatenate
                                           'string
                                           result
                                           "-"
                                           (find-fq-class-name-by-class-name (ast-value child "name") ast))))))))
          (return-from get-fq-name-of-declaration result))

        (loop for child in (jsown:val ast "children")
              do (setf stack (append stack (list child))))))))

(defmethod find-class-hierarchy-generic ((traversal traversal-java)
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

    (when (equal (ast-value ast "type") "PACKAGE")
      (unless (equal (ast-value ast "packageName") target-package-name)
        (return-from find-class-hierarchy-generic)))
    (when (equal (ast-value ast "type") "CLASS")
      (unless (equal (ast-value ast "name") target-class-name)
        (return-from find-class-hierarchy-generic))
      (return-from find-class-hierarchy-generic
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

