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
          (let ((dot-expressions (get-dot-expressions ast nil)))
            (when (equal (car (last dot-expressions)) target-name)
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
                (setf result dot-expressions-name)))))

        (when (equal (cdar ast) "kotlin.FILE")
          (loop for child in (jsown:val ast "children")
                with class-name = (first (split #\. result))
                do (when (equal (jsown:val child "type") "IMPORT_LIST")
                     (loop for child in (jsown:val child "children")
                           do (when (and
                                      (equal (jsown:val child "type") "IMPORT_DIRECTIVE")
                                      (equal (car (last (split #\. (jsown:val child "fqName")))) class-name))
                                (let ((split-import-names (split #\. (jsown:val child "fqName")))
                                      fq-name)
                                  (setf fq-name 
                                        (format nil "~{~a~^.~}"
                                                (append (subseq split-import-names
                                                                0 
                                                                (1- (length split-import-names)))
                                                        (list result))))
                                  (when (equal fq-name (cdr (assoc :fq-name target-pos)))
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

(defun get-dot-expressions (ast results)
  (when (equal (cdar ast) "VALUE_ARGUMENT_LIST")
    (return-from get-dot-expressions results))

  (when (equal (cdar ast) "REFERENCE_EXPRESSION")
    (setf results (append results (list (jsown:val ast "name")))))

  (loop for child in (jsown:val ast "children")
        do
        (setf results (get-dot-expressions (cdr child) results)))
  results)

