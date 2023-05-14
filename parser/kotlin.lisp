(defpackage #:inga/parser/kotlin
  (:use #:cl
        #:inga/parser/base
        #:inga/utils)
  (:import-from #:inga/cache
                #:put-value
                #:get-value)
  (:export #:parser-kotlin))
(in-package #:inga/parser/kotlin)

(defclass parser-kotlin (parser)
  ())

(defmethod start-parser ((parser parser-kotlin) include exclude)
  (setf (parser-process parser)
        (uiop:launch-program
          (format nil "java -cp ~a/libs/ktparser.jar inga.Main"
                  (uiop:getenv "INGA_HOME"))
          :input :stream :output :stream))
  (create-indexes parser '("*.kt") exclude))

(defmethod stop-parser ((parser parser-kotlin))
  (clean-indexes)
  (uiop:close-streams (parser-process parser)))

(defmethod find-affected-pos ((parser parser-kotlin) src-path ast line-no)
  (let ((q (make-queue))
        (ast-pos (cdr (assoc :pos (convert-to-ast-pos
                                    (parser-path parser)
                                    (list
                                      (cons :path src-path)
                                      (cons :line line-no)
                                      (cons :offset -1)))))))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (and
                (string= (cdr (car ast)) "FUN")
                (<= (jsown:val (jsown:val ast "textRange") "startOffset") ast-pos)
                (>= (jsown:val (jsown:val ast "textRange") "endOffset") ast-pos))
          (when (jsown:keyp ast "name")
            (let ((name (jsown:val ast "name")))
              (return (convert-to-pos (parser-path parser) src-path
                                      name
                                      (when (jsown:keyp ast "fqName") (jsown:val ast "fqName"))
                                      (jsown:val (jsown:val ast "textRange") "startOffset"))))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children") do
                (enqueue q (cdr child))))))))

(defmethod find-entrypoint ((parser parser-kotlin) pos))

(defmethod matches-reference-name ((parser parser-kotlin) ast target-name)
  (and
    (equal (cdar ast) "REFERENCE_EXPRESSION")
    (equal (jsown:val ast "name") target-name)))

(defmethod find-reference-pos ((parser parser-kotlin) index-path root-ast ast target-pos)
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
                    (convert-to-pos
                      (parser-path parser)
                      (get-original-path index-path)
                      nil
                      nil
                      (jsown:val found-ast "textOffset"))))
                (setf result dot-expressions-name))))
          (loop for child in (jsown:val ast "children")
                do
                (when (equal (jsown:val child "type") "REFERENCE_EXPRESSION")
                  (setf target-name (jsown:val child "name"))))
          )

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
                                (convert-to-pos
                                  (parser-path parser)
                                  (get-original-path index-path)
                                  nil
                                  nil
                                  (jsown:val found-ast "textOffset"))))))))))

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

