(defpackage #:inga/ast-analyzer/kotlin
  (:use #:cl
        #:inga/ast-analyzer/base
        #:inga/utils)
  (:export #:ast-analyzer-kotlin))
(in-package #:inga/ast-analyzer/kotlin)

(defclass ast-analyzer-kotlin (ast-analyzer)
  ())

(defmethod start-ast-analyzer ((kind (eql :kotlin)) exclude path)
  (setf *ast-analyzers*
        (acons :kotlin
               (make-instance
                 'ast-analyzer-kotlin
                 :process
                 (uiop:launch-program
                   (format nil "java -cp ~a/libs/ktparser.jar inga.Main"
                           (uiop:getenv "INGA_HOME"))
                   :input :stream :output :stream)
                 :path path) 
               *ast-analyzers*))
  (create-indexes (cdr (assoc :kotlin *ast-analyzers*)) '("*.kt") exclude)
  (cdr (assoc :kotlin *ast-analyzers*)))

(defmethod stop-ast-analyzer ((ast-analyzer ast-analyzer-kotlin))
  (clean-indexes)
  (uiop:close-streams (ast-analyzer-process ast-analyzer)))

(defmethod find-definitions-generic ((ast-analyzer ast-analyzer-kotlin) range)
  (let ((q (make-queue))
        (src-path (cdr (assoc :path range)))
        (index-path (get-index-path (cdr (assoc :path range))))
        (start-offset (cdr (assoc :start-offset range)))
        (end-offset (cdr (assoc :end-offset range)))
        ast
        results)
    (handler-case
      (setf ast (cdr (jsown:parse (uiop:read-file-string index-path))))
      (error (e)
             (format t "~a~%" e)
             (return-from find-definitions-generic)))
    (enqueue q ast)
    (loop
      (setf ast (dequeue q))
      (if (null ast) (return))

      (when (and
              (string= (cdr (car ast)) "FUN")
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

      (when (jsown:keyp ast "children")
        (loop for child in (jsown:val ast "children")
              do (enqueue q (cdr child)))))
    results))

(defmethod find-reference ((ast-analyzer ast-analyzer-kotlin) target-pos ast index-path)
  (let ((fq-name (find-fq-name-for-reference ast)))
    (unless fq-name (return-from find-reference))

    (alexandria:switch ((cdr (assoc :type target-pos)))
      (:rest-server
        ;; TODO: implementes
        nil)
      (t
        (when (equal fq-name (cdr (assoc :fq-name target-pos)))
          (list
            (cons :path (get-original-path index-path))
            (cons :top-offset (ast-value ast "textOffset"))))))))

(defun find-fq-name-for-reference (ast)
  (alexandria:switch ((ast-value ast "type") :test #'equal)
    ("CALL_EXPRESSION"
     (let ((root (first (ast-get ast '("DOT_QUALIFIED_EXPRESSION") :direction :upward))))
       (format nil "~a.~a"
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
               (ast-value (first (ast-get ast '("REFERENCE_EXPRESSION"))) "name"))))))

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

