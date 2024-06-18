(defpackage #:inga/plugin/jvm-helper
  (:use #:cl)
  (:import-from #:inga/cache
                #:defunc)
  (:import-from #:inga/logger
                #:log-error)
  (:export #:find-base-path
           #:is-primitive-type
           #:convert-to-json-type))
(in-package #:inga/plugin/jvm-helper)

(defunc find-base-path (path)
  (when (equal path #p"/")
    (return-from find-base-path))

  (let ((parent-path (uiop:pathname-parent-directory-pathname path)))
    (loop for file in (uiop:directory-files parent-path)
          do
          (when (find (file-namestring file) '("pom.xml" "build.gradle" "build.gradle.kts") :test #'equal)
            (return-from find-base-path parent-path)))
    (find-base-path parent-path)))

(defun is-primitive-type (class-name)
  (find class-name '("BOOLEAN"
                     "BYTE"
                     "SHORT"
                     "INT"
                     "LONG"
                     "CHAR"
                     "FLOAT"
                     "DOUBLE"
                     ) :test 'equal))

(defun convert-to-json-type (type)
  (cond
    ((or (equal type "BOOLEAN")
         (equal type "java.lang.Boolean"))
     "boolean")
    ((or (equal type "INT")
         (equal type "LONG")
         (equal type "FLOAT")
         (equal type "DOUBLE")
         (equal type "java.lang.Integer")
         (equal type "java.lang.Long") 
         (equal type "java.lang.Float")
         (equal type "java.lang.Double"))
     "number")
    ((equal type "java.lang.String")
     "string")
    (t
     (log-error (format nil "unexpected type: ~a" type)))))

