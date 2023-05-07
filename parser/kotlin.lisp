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

(defmethod find-caller ((parser parser-kotlin) index-path ast pos)
  (let ((q (make-queue))
        (target-fq-name (cdr (assoc :fq-name pos)))
        (target-name (cdr (assoc :name pos)))
        imported-name
        is-found-import
        results)
    (setf imported-name
          (concatenate 'string (car (last (split #\. target-fq-name))) "." target-name))
    (setf is-found-import (not (null (find target-fq-name (find-import-list (get-original-path index-path) ast) :test #'equal))))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)) result)
        (if (null ast) (return))

        (when (string= (cdar ast) "DOT_QUALIFIED_EXPRESSION")
          (let ((fq-name (find-fq-method-name parser (get-original-path index-path) ast ast nil)))
            (when (or
                    (and is-found-import (string= fq-name imported-name))
                    (string= fq-name (concatenate 'string target-fq-name "." target-name)))
              (setf results (append results (list (convert-to-pos
                                                    (parser-path parser)
                                                    (get-original-path index-path)
                                                    nil nil
                                                    (jsown:val (jsown:val ast "textRange") "startOffset"))))))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children")
                do (enqueue q (cdr child))))))
    results))

(defmethod find-fq-method-name ((parser parser-kotlin) src-path root-ast ast result)
  (when (string= (cdar ast) "REFERENCE_EXPRESSION")
    (setf result (concatenate 'string
                              (if result (concatenate 'string result ".") "")
                              (jsown:val (cdr ast) "name"))))
  (loop for child in (jsown:val ast "children")
        do (setf result (find-fq-method-name parser src-path root-ast (cdr child) result)))
  result)

(defun find-import-list (src-path ast)
  (let ((q (make-queue)))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (string= (cdr (car ast)) "IMPORT_LIST")
          (return
            (mapcar (lambda (child)
                      (jsown:val child "fqName"))
                    (jsown:val ast "children"))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children")
                do (enqueue q (cdr child))))))))

