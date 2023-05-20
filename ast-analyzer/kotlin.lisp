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

(defmethod find-affected-poss ((ast-analyzer ast-analyzer-kotlin) range)
  (let ((q (make-queue))
        (src-path (cdr (assoc :path range)))
        (index-path (get-index-path (cdr (assoc :path range))))
        (start-offset (cdr (assoc :start-offset range)))
        (end-offset (cdr (assoc :end-offset range)))
        ast
        results)
    (setf ast (cdr (jsown:parse (uiop:read-file-string index-path))))
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

(defmethod matches-reference-name ((ast-analyzer ast-analyzer-kotlin) ast target-name)
  (and
    (equal (cdar ast) "REFERENCE_EXPRESSION")
    (equal (jsown:val ast "name") target-name)))

(defmethod find-reference-pos ((ast-analyzer ast-analyzer-kotlin) index-path root-ast ast target-pos)
  (let ((q (make-queue))
        (target-name (cdr (assoc :name target-pos)))
        (found-ast ast)
        result)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (equal (cdar ast) "DOT_QUALIFIED_EXPRESSION")
          (let ((dot-expressions (get-dot-expressions ast)))
            (when (equal (car (last dot-expressions)) target-name)
              (setf result target-name)
              (let ((dot-expressions-name (format nil "~{~a~^.~}" dot-expressions)))
                (when (equal dot-expressions-name (cdr (assoc :fq-name target-pos)))
                  (return-from
                    find-reference-pos
                    (list
                      (cons :path (get-original-path index-path))
                      (cons :top-offset (jsown:val found-ast "textOffset")))))
                (setf result dot-expressions-name))))
          (loop for child in (jsown:val ast "children")
                do
                (when (equal (jsown:val child "type") "REFERENCE_EXPRESSION")
                  (setf target-name (jsown:val child "name")))))

        (when (equal (cdar ast) "CLASS")
          (loop for child in (jsown:val ast "children")
                do
                (when (equal (jsown:val child "type") "PRIMARY_CONSTRUCTOR")
                  (let ((found-type (find-type child target-name)))
                    (when found-type
                      (setf result (format nil "~{~a~^.~}"
                                           (remove nil (list found-type result)))))))))

        (when (equal (cdar ast) "kotlin.FILE")
          (loop for child in (jsown:val ast "children")
                with class-name = (first (split #\. result))
                do
                (when (equal (jsown:val child "type") "IMPORT_LIST")
                  (loop for child in (jsown:val child "children")
                        do
                        (when (and
                                (equal (jsown:val child "type") "IMPORT_DIRECTIVE")
                                (equal (car (last (split #\. (jsown:val child "fqName")))) class-name))
                          (let ((split-import-names (split #\. (jsown:val child "fqName")))
                                (target-fq-name (cdr (assoc :fq-name target-pos)))
                                fq-name)
                            (setf fq-name 
                                  (format nil "~{~a~^.~}"
                                          (append (subseq split-import-names
                                                          0 
                                                          (1- (length split-import-names)))
                                                  (list result))))
                            (when (equal fq-name target-fq-name)
                              (return-from
                                find-reference-pos
                                (list
                                  (cons :path (get-original-path index-path))
                                  (cons :top-offset (jsown:val found-ast "textOffset")))))))))))

        (when (jsown:keyp ast "parent")
          (enqueue q (jsown:val ast "parent")))))))

(defun get-dot-expressions (ast)
  (multiple-value-bind (results calls)
    (get-dot-expressions-recursive ast)
    (if (eq (length calls) 1)
        (last results)
        results)))

(defun get-dot-expressions-recursive (ast &optional calls results)
  (when (equal (cdar ast) "VALUE_ARGUMENT_LIST")
    (return-from get-dot-expressions-recursive (values results calls)))

  (when (equal (cdar ast) "CALL_EXPRESSION")
    (push ast calls))

  (when (equal (cdar ast) "REFERENCE_EXPRESSION")
    (setf results (append results (list (jsown:val ast "name")))))

  (loop for child in (jsown:val ast "children")
        do
        (multiple-value-bind (ret-results ret-calls)
          (get-dot-expressions-recursive (cdr child) calls results)
          (progn
            (setf results ret-results)
            (setf calls ret-calls))))
  (values results calls))

(defun find-type (ast target-variable &optional found-target result)
  (when (and
          (equal (jsown:val ast "type") "VALUE_PARAMETER")
          (equal (jsown:val ast "name") target-variable))
    (setf found-target t))

  (when (and
          found-target
          (equal (jsown:val ast "type") "REFERENCE_EXPRESSION"))
    (setf result (jsown:val ast "name")))

  (loop for child in (jsown:val ast "children")
        do
        (setf result (find-type child target-variable found-target result)))
  result)

