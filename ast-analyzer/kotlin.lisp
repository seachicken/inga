(defpackage #:inga/ast-analyzer/kotlin
  (:use #:cl
        #:inga/ast-analyzer/base
        #:inga/utils)
  (:import-from #:inga/cache
                #:put-value
                #:get-value)
  (:export #:ast-analyzer-kotlin))
(in-package #:inga/ast-analyzer/kotlin)

(defclass ast-analyzer-kotlin (ast-analyzer)
  ())

(defmethod start-ast-analyzer ((ast-analyzer ast-analyzer-kotlin) include exclude)
  (setf (ast-analyzer-process ast-analyzer)
        (uiop:launch-program
          (format nil "java -cp ~a/libs/ktparser.jar inga.Main"
                  (uiop:getenv "INGA_HOME"))
          :input :stream :output :stream))
  (create-indexes ast-analyzer '("*.kt") exclude))

(defmethod stop-ast-analyzer ((ast-analyzer ast-analyzer-kotlin))
  (clean-indexes)
  (uiop:close-streams (ast-analyzer-process ast-analyzer)))

(defmethod find-definitions ((ast-analyzer ast-analyzer-kotlin) range)
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
             (return-from find-definitions)))
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

(defmethod find-entrypoint ((ast-analyzer ast-analyzer-kotlin) pos))

(defmethod find-reference ((ast-analyzer ast-analyzer-kotlin) target-pos ast index-path)
  (let ((fq-name (find-fq-name-for-reference ast-analyzer ast)))
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

(defmethod find-fq-name-for-reference ((ast-analyzer ast-analyzer-kotlin) ast)
  (alexandria:switch ((ast-value ast "type") :test #'equal)
    ("CALL_EXPRESSION"
     (let ((root (first (ast-get ast '("DOT_QUALIFIED_EXPRESSION") :direction :upward))))
       (format nil "~a.~a"
               (if (ast-get root '("DOT_QUALIFIED_EXPRESSION"))
                   (let (fq-class-names)
                     (push (ast-value (first (ast-get root '("DOT_QUALIFIED_EXPRESSION"
                                                             "CALL_EXPRESSION"
                                                             "REFERENCE_EXPRESSION")))
                                      "name")
                           fq-class-names)
                     (push (ast-value (first (ast-get root '("DOT_QUALIFIED_EXPRESSION"
                                                             "DOT_QUALIFIED_EXPRESSION"
                                                             "REFERENCE_EXPRESSION")))
                                      "name")
                           fq-class-names)
                     (setf fq-class-names
                           (append 
                             (mapcar (lambda (ast) (ast-value ast "name"))
                                     (ast-get root '("DOT_QUALIFIED_EXPRESSION"
                                                     "DOT_QUALIFIED_EXPRESSION"
                                                     "DOT_QUALIFIED_EXPRESSION"
                                                     "REFERENCE_EXPRESSION")))
                             fq-class-names))
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

