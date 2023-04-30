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

(defmethod start-parser ((parser parser-kotlin))
  (setf (parser-process parser)
        (uiop:launch-program
          (format nil "java -cp ~a/libs/ktparser.jar inga.Main"
                  (uiop:getenv "INGA_HOME"))
          :input :stream :output :stream)))

(defmethod stop-parser ((parser parser-kotlin))
  (uiop:close-streams (parser-process parser)))

(defmethod exec-parser ((parser parser-kotlin) file-path)
  (let ((path (namestring
                (uiop:merge-pathnames* file-path (parser-path parser))))
        cache
        ast)
    (setf cache (get-value (parser-cache parser) (get-parse-key path)))
    (values
      (if cache
          (when (> (length cache) 0)
            (cdr (jsown:parse cache)))
          (progn
            (setf ast (exec-command parser path))
            (put-value (parser-cache parser) (get-parse-key path) ast)
            (when (> (length ast) 0)
              (cdr (jsown:parse ast)))))
      (when cache t))))

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

