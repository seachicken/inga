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

(defmethod find-references ((parser parser-kotlin) pos)
  (let ((target-package (split #\. (cdr (assoc :fq-name pos)))))
    (loop for path in (uiop:directory-files *index-path*)
          with results
          with ast
          with target = (format nil "~{~a~^.~}"
                                (subseq target-package 0 (- (length target-package) 1)))
          do (progn
               (setf ast (cdr (jsown:parse (uiop:read-file-string path))))
               (when (find target (find-import-list ast) :test #'equal)
                 (let ((callers (find-caller ast pos))
                       (org-path (format nil "~{~a~^/~}"
                                         (subseq (split #\/ (ppcre:regex-replace-all
                                                              "--"
                                                              (enough-namestring path)
                                                              "/"))
                                                 1))))
                   (setf results (append results
                                         (mapcar (lambda (caller)
                                                   (convert-to-pos
                                                     (parser-path parser) org-path
                                                     (jsown:val caller "name") nil
                                                     (jsown:val (jsown:val caller "textRange") "startOffset")))
                                                 callers))))))
          finally (return results))))

(defun find-import-list (ast)
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

(defun find-caller (ast pos)
  (let ((q (make-queue)) (target (cdr (assoc :name pos))) results)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (string= (cdr (car ast)) "DOT_QUALIFIED_EXPRESSION")
          (loop for child in (jsown:val ast "children")
                do (loop for child in (jsown:val child "children")
                         do (when (and
                                    (string= (cdadr child) "REFERENCE_EXPRESSION")
                                    (string= (jsown:val (cdr child) "name") target))
                              (setf results (append results (list (cdr child))))))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children")
                do (enqueue q (cdr child))))))
    results))
