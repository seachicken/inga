(defpackage #:inga/analyzer/java
  (:use #:cl
        #:inga/analyzer/base
        #:inga/utils)
  (:import-from #:alexandria
                #:switch)
  (:import-from #:inga/ast-index
                #:ast-index-paths
                #:ast-index-root-path
                #:create-indexes
                #:get-ast
                #:stop-indexes)
  (:import-from #:inga/analyzer/kotlin
                #:analyzer-kotlin)
  (:import-from #:inga/file
                #:get-file-type)
  (:import-from #:inga/plugin/jvm-dependency-loader
                #:load-hierarchy 
                #:load-signatures)
  (:import-from #:inga/plugin/jvm-helper
                #:find-base-path
                #:is-primitive-type)
  (:export #:analyzer-java
           #:find-definition))
(in-package #:inga/analyzer/java)

(defclass analyzer-java (analyzer)
  ())

(defmethod start-analyzer ((kind (eql :java)) include exclude path index)
  (setf *analyzers*
        (acons :java
               (make-instance 'analyzer-java
                              :path path
                              :index index)
               *analyzers*))
  (create-indexes index :java include '("*.java") exclude)
  (create-index-groups (cdr (assoc :java *analyzers*)))
  (cdr (assoc :java *analyzers*)))

(defmethod stop-analyzer ((analyzer analyzer-java))
  (stop-indexes (analyzer-index analyzer))
  (setf *analyzers* nil))

(defun create-index-groups (analyzer)
  (loop for path in (remove-if-not (lambda (p) (eq (get-file-type p) :java))
                                   (ast-index-paths (analyzer-index analyzer)))
        do (set-index-group analyzer path)))

(defmethod set-index-group ((analyzer analyzer-java) path)
  (let ((index-key (find-package-index-key (get-ast (analyzer-index analyzer) path))))
    (unless (gethash :package *file-index*)
      (setf (gethash :package *file-index*) (make-hash-table)))
    (push path (gethash index-key (gethash :package *file-index*))))
  (let ((index-key (find-project-index-key
                      (merge-pathnames path (ast-index-root-path (analyzer-index analyzer))))))
    (unless (gethash :module *file-index*)
      (setf (gethash :module *file-index*) (make-hash-table)))
    (push path (gethash index-key (gethash :module *file-index*)))))

(defun find-package-index-key (ast)
  (loop
    with stack = (list ast)
    do
    (setf ast (pop stack))
    (unless ast (return))

    (when (equal (ast-value ast "type") "PACKAGE")
      (return-from find-package-index-key (intern (ast-value ast "packageName") :keyword)))

    (loop for child in (jsown:val ast "children")
          do (setf stack (append stack (list child))))))

(defun find-project-index-key (path)
  (let ((base-path (find-base-path path)))
    (when base-path (intern (namestring base-path) :keyword))))

(defmethod get-scoped-index-paths-generic ((analyzer analyzer-java) pos)
  (cond
    ((eq (cdr (assoc :type pos)) :module-private)
     (list (cdr (assoc :path pos))))
    ((eq (cdr (assoc :type pos)) :module-default)
     (gethash (find-package-index-key (get-ast (analyzer-index analyzer)
                                               (cdr (assoc :path pos))))
              (gethash :package *file-index*)))
    ((eq (cdr (assoc :type pos)) :module-public)
     (gethash (find-project-index-key (merge-pathnames (cdr (assoc :path pos))
                                                       (analyzer-path analyzer)))
              (gethash :module *file-index*)))
    (t
     (ast-index-paths (analyzer-index analyzer)))))

(defmethod find-definitions-generic ((analyzer analyzer-java) range)
  (let ((q (make-queue))
        (src-path (cdr (assoc :path range)))
        (path (cdr (assoc :path range)))
        (start-offset (cdr (assoc :start-offset range)))
        (end-offset (cdr (assoc :end-offset range)))
        results)
    (loop
      with ast = (get-ast (analyzer-index analyzer) path)
      initially (enqueue q ast)
      do
      (setf ast (dequeue q))
      (unless ast (return))

      (when (and (or
                   (and
                     (jsown:keyp ast "parent")
                     (equal (ast-value (jsown:val ast "parent") "type") "CLASS")
                     (equal (ast-value ast "type") "VARIABLE"))
                   (equal (ast-value ast "type") "METHOD")
                   (equal (ast-value ast "type") "BLOCK"))
                 (contains-offset (jsown:val ast "startPos") (jsown:val ast "endPos")
                                  start-offset end-offset)
                 (jsown:keyp ast "name"))
        (push
          `((:type . ,(get-scope ast))
            (:path . ,src-path)
            (:name . ,(jsown:val ast "name"))
            (:fq-name . ,(find-fq-name ast path))
            (:top-offset . ,(jsown:val ast "pos")))
          results))

      (loop for child in (jsown:val ast "children") do (enqueue q child)))
    results))

(defun get-scope (ast)
  (let ((modifiers (split-trim-comma (ast-value (first (get-asts ast '("MODIFIERS"))) "name"))))
    (cond
      ((find "PUBLIC" modifiers :test #'equal) :module-public)
      ((find "PROTECTED" modifiers :test #'equal) :module-protected)
      ((find "PRIVATE" modifiers :test #'equal) :module-private)
      (t :module-default))))

(defmethod find-reference ((analyzer analyzer-java) target-pos fq-name ast path)
  (when (and (or (equal (ast-value ast "type") "ASSIGNMENT")
                 (equal (ast-value ast "type") "NEW_CLASS")
                 (equal (ast-value ast "type") "METHOD_INVOCATION"))
             (find-signature fq-name
                             #'(lambda (fqcn) (list target-pos))
                             path))
    `((:path . ,path)
      (:top-offset . ,(ast-value ast "pos")))))

(defmethod find-fq-name-generic ((analyzer analyzer-java) ast path)
  (cond
    ((equal (ast-value ast "type") "ASSIGNMENT")
     (let* ((lhs (first (get-asts ast '("MEMBER_SELECT"))))
            (v (when lhs (find-definition (ast-value lhs "name") lhs))))
       (when v (find-fq-name-generic analyzer v path))))
    ((equal (ast-value ast "type") "NEW_CLASS")
     (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
             (find-fq-class-name ast path)
             ;; get inner class name
             (first (last (split #\. (ast-value ast "name"))))
             (mapcar (lambda (arg) (find-fq-class-name arg path))
                     (get-asts ast '("*")))))
    ((and (equal (ast-value ast "type") "METHOD_INVOCATION")
          (get-asts ast '("MEMBER_SELECT")))
     (format nil "~a.~a~:[~;-~]~:*~{~a~^-~}"
             (cond
               ;; if method chain
               ((get-asts ast '("MEMBER_SELECT" "METHOD_INVOCATION"))
                (let* ((fq-name (find-fq-name-generic
                                  analyzer
                                  (first (get-asts ast '("MEMBER_SELECT" "METHOD_INVOCATION")))
                                  path))
                       (method (find-signature
                                 fq-name
                                 #'(lambda (fqcn) (load-signatures fqcn path))
                                 path)))
                  (when method (cdr (assoc :return method)))))
               ((get-asts ast '("MEMBER_SELECT" "IDENTIFIER"))
                (find-fq-class-name
                  (first (get-asts ast '("MEMBER_SELECT" "IDENTIFIER")))
                  path))
               ((get-asts ast '("MEMBER_SELECT" "NEW_CLASS"))
                (find-fq-class-name
                  (first (get-asts ast '("MEMBER_SELECT" "NEW_CLASS"))) path))
               ((get-asts ast '("MEMBER_SELECT" "MEMBER_SELECT"))
                (find-fq-class-name
                  (first (get-asts ast '("MEMBER_SELECT" "MEMBER_SELECT"))) path)))
             (ast-value (first (get-asts ast '("*"))) "name")
             (mapcar (lambda (arg) (find-fq-class-name arg path))
                     (nthcdr 1 (get-asts ast '("*"))))))
    (t
     (loop
       with q = (make-queue)
       with fq-names
       with class-names
       initially
       (enqueue q ast)
       (setf fq-names
             (list
               (cond
                 ((equal (ast-value ast "type") "METHOD")
                  (loop for child in (jsown:val ast "children")
                        with method-with-params = (ast-value ast "name")
                        do
                        (when (equal (jsown:val child "type") "VARIABLE")
                          (loop for child in (jsown:val child "children")
                                do
                                (when (and (ast-value child "name")
                                           (not (equal (jsown:val child "type") "MODIFIERS"))
                                           (not (equal (ast-value child "name") "")))
                                  (setf method-with-params
                                        (concatenate
                                          'string
                                          method-with-params
                                          "-"
                                          (find-fq-class-name child path))))))
                        finally (return method-with-params)))
                 ((equal (ast-value ast "type") "METHOD_INVOCATION")
                  (loop for child in (jsown:val ast "children")
                        for i from 0
                        with method-with-params = (ast-value (first (get-asts ast '("IDENTIFIER")))
                                                             "name")
                        do
                        (when (> i 0)
                          (setf method-with-params
                                (concatenate
                                  'string
                                  method-with-params
                                  "-"
                                  (find-fq-class-name child path))))
                        finally (return method-with-params)))
                 (t
                  (ast-value ast "name")))))
       do
       (setf ast (dequeue q))
       (unless ast (return (format nil "~{~a~^.~}" fq-names)))

       (when (and
               fq-names
               (or
                 (equal (ast-value ast "type") "CLASS")
                 (equal (ast-value ast "type") "INTERFACE")))
         (setf class-names (append (list (ast-value ast "name")) class-names)))

       (when (and
               fq-names
               (equal (ast-value ast "type") "COMPILATION_UNIT"))
         (setf fq-names (append (list (format nil "~{~a~^$~}" class-names)) fq-names))
         (setf fq-names (append
                          (list (ast-value (first (get-asts ast '("PACKAGE"))) "packageName"))
                          fq-names)))

       (when (jsown:keyp ast "parent")
         (enqueue q (jsown:val ast "parent")))))))

(defmethod find-fq-class-name-generic ((analyzer analyzer-java) ast path)
  (let ((name (ast-value ast "name")))
    (cond
      ((uiop:string-suffix-p (ast-value ast "type") "_LITERAL")
       (if (equal (ast-value ast "type") "STRING_LITERAL")
           "java.lang.String"
           (subseq (ast-value ast "type") 0 (- (length (ast-value ast "type")) 8))))
      ((equal (ast-value ast "type") "MEMBER_SELECT")
       (cond
         ((equal name "class")
          "java.lang.Class")
         ((equal name "length")
          "INT")
         (t
          (find-fq-class-name-generic analyzer (first (get-asts ast '("IDENTIFIER"))) path))))
      ((equal (ast-value ast "type") "VARIABLE")
       (find-fq-class-name-generic analyzer (second (get-asts ast '("*"))) path))
      ((equal (ast-value ast "type") "METHOD_INVOCATION")
       (let* ((fq-name (find-fq-name ast path))
              (method (find-signature fq-name
                                      #'(lambda (fqcn) (load-signatures fqcn path))
                                      path)))
         (when method (cdr (assoc :return method)))))
      (t
       (let ((def (find-definition name ast)))
         (if def
             (find-fq-class-name-generic analyzer def path)
             (concatenate
               'string 
               (cond
                 ((is-primitive-type name)
                  name)
                 ;; FIXME: support for all types
                 ((find name '("Long" "String") :test 'equal)
                  (concatenate 'string "java.lang." name))
                 ((equal name "class")
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
                                             (get-asts ast '("IMPORT"))
                                             (concatenate 'string "." name)
                                             :key-name "fqName"))))
                        (setf fq-names
                              (if import
                                  (list (ast-value import "fqName"))
                                  (append
                                    (list (ast-value (first (get-asts ast '("PACKAGE"))) "packageName"))
                                    fq-names)))))

                    (when (equal (ast-value ast "type") "CLASS")
                      (if (or (filter-by-name (get-asts ast '("CLASS")) name)
                              (filter-by-name (get-asts ast '("ENUM")) name))
                          ;; inner class
                          (push (concatenate 'string (ast-value ast "name") "$" name) fq-names)
                          (push (format nil "~{~a~^$~}" (split #\. name)) fq-names)))

                    (when (jsown:keyp ast "parent")
                      (enqueue q (jsown:val ast "parent"))))))
               (if (equal (ast-value ast "type") "ARRAY_TYPE") "[]" ""))))))))

(defun find-definition (variable-name ast &optional visitor)
  (unless variable-name
    (return-from find-definition))

  (labels ((find-chain-root (ast)
             (if (get-asts ast '("MEMBER_SELECT" "METHOD_INVOCATION"))
                 (find-chain-root (first (get-asts ast '("MEMBER_SELECT" "METHOD_INVOCATION"))))
                 ast)))
    (loop
      with q = (make-queue)
      with has-this = (equal (ast-value (first (get-asts ast '("IDENTIFIER"))) "name") "this")
      initially (enqueue q ast)
      do
      (setf ast (dequeue q))
      (when (null ast) (return))
      (when visitor
        (loop for ast in (remove-if (lambda (sibling)
                                      (> (ast-value sibling "pos") (ast-value ast "pos")))
                                    (get-asts ast '("*") :direction :horizontal))
              do (funcall visitor ast)))

      (let ((variable (first (if (equal (ast-value ast "type") "VARIABLE")
                                 (filter-by-name (list ast) variable-name)
                                 (filter-by-name (get-asts ast '("VARIABLE")) variable-name)))))
        (when (and variable
                   (if has-this (not (equal (ast-value ast "type") "METHOD")) t))
          (return variable)))

      (let ((lambda-exp (first (get-asts ast '("LAMBDA_EXPRESSION") :direction :upward))))
        (when (get-asts lambda-exp '("VARIABLE"))
          (let* ((root (find-chain-root (first (get-asts ast '("LAMBDA_EXPRESSION"
                                                               "METHOD_INVOCATION")
                                                         :direction :upward))))

                 (v (find-definition
                      (ast-value (first (get-asts root '("MEMBER_SELECT" "IDENTIFIER"))) "name")
                      root)))
            (return (second (if v
                                (get-asts v '("METHOD_INVOCATION" "*"))
                                (get-asts root '("*"))))))))

      (enqueue q (ast-value ast "parent")))))

(defmethod find-reference-to-literal-generic ((analyzer analyzer-java) ast path)
  (cond
    ((equal (ast-value ast "type") "STRING_LITERAL")
     `(((:path . ,path)
        (:name . ,(ast-value ast "name"))
        (:top-offset . ,(ast-value ast "pos")))))
    ((or (equal (ast-value ast "type") "IDENTIFIER")
         (equal (ast-value ast "type") "VARIABLE"))
     (loop
       with q = (make-queue)
       with variable-name = (ast-value ast "name")
       initially (enqueue q ast)
       do
       (setf ast (dequeue q))
       (unless ast (return))
       
       (when (equal (ast-value ast "type") "CLASS")
         (let* ((v (first (filter-by-name (get-asts ast '("VARIABLE")) variable-name)))
                (refs (when v (find-references (first (find-definitions-generic
                                                        analyzer
                                                        `((:path . ,path)
                                                          (:start-offset . ,(jsown:val v "pos"))
                                                          (:end-offset . ,(jsown:val v "pos")))))
                                               (analyzer-index analyzer)))))
           (return-from
             find-reference-to-literal-generic
             (if (get-asts v '("STRING_LITERAL"))
                 (find-reference-to-literal-generic
                   analyzer (first (get-asts v '("STRING_LITERAL"))) path)
                 (mapcan (lambda (ref)
                           (let ((ast (find-ast ref (analyzer-index analyzer))))
                             (when ast
                               (find-reference-to-literal-generic
                                 analyzer
                                 (first (get-asts ast '("IDENTIFIER")))
                                 (cdr (assoc :path ref))))))
                         refs)))))
       (when (and (equal (ast-value ast "type") "METHOD")
                  (filter-by-name (get-asts ast '("VARIABLE")) variable-name))
         (let ((refs (find-references (first (find-definitions-generic
                                               analyzer
                                               `((:path . ,path)
                                                 (:start-offset . ,(jsown:val ast "pos"))
                                                 (:end-offset . ,(jsown:val ast "pos")))))
                                      (analyzer-index analyzer))))
           (return-from
             find-reference-to-literal-generic
             (mapcan (lambda (ref)
                       (find-reference-to-literal-generic
                         analyzer
                         (first (get-asts (find-ast ref (analyzer-index analyzer))
                                          '("METHOD_INVOCATION"
                                            "MEMBER_SELECT"
                                            "NEW_CLASS"
                                            "STRING_LITERAL")))
                         (cdr (assoc :path ref))))
                     refs))))

    (enqueue q (ast-value ast "parent"))))))

(defmethod find-caller-generic ((analyzer analyzer-java) fq-names ast path)
  (labels ((find-caller (fq-names ast path)
             (unless ast (return-from find-caller))

             (let* ((found-fq-name (find-fq-name ast path))
                    (matched-api (find-signature found-fq-name
                                                 #'(lambda (fqcn) fq-names)
                                                 path)))
               (if matched-api
                   (values ast matched-api)
                   (if (get-asts ast '("MEMBER_SELECT" "METHOD_INVOCATION"))
                     (find-caller
                       fq-names
                       (first (get-asts ast '("MEMBER_SELECT" "METHOD_INVOCATION")))
                       path)
                     (progn
                       (find-definition
                         (ast-value (first (get-asts ast '("MEMBER_SELECT" "IDENTIFIER"))) "name")
                         ast
                         #'(lambda (ast)
                             (multiple-value-bind (caller api)
                               (find-caller
                                 fq-names
                                 (first (get-asts ast '("METHOD_INVOCATION")))
                                 path)
                               (when caller
                                 (return-from find-caller (values caller api))))))
                       nil))))))
    (find-caller fq-names ast path)))

(defmethod find-class-hierarchy-generic ((analyzer analyzer-java)
                                         fq-class-name root-ast path)
  (load-hierarchy fq-class-name path))

