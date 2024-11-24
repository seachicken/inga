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

(defunc find-base-path (path root-path)
  (let ((full-path (merge-pathnames path root-path)))
    (let ((parent-full-path (uiop:pathname-parent-directory-pathname full-path)))
      (unless (uiop:string-prefix-p (namestring root-path) (namestring parent-full-path))
        (return-from find-base-path))

      (loop for file in (uiop:directory-files parent-full-path)
            do
            (when (find (file-namestring file) '("pom.xml" "build.gradle" "build.gradle.kts") :test #'equal)
              (return-from find-base-path parent-full-path)))
      (find-base-path (enough-namestring parent-full-path root-path) root-path))))

(defun is-primitive-type (class-name)
  (find class-name '("BOOLEAN"
                     "BYTE"
                     "CHAR"
                     "DOUBLE"
                     "FLOAT"
                     "INT"
                     "LONG"
                     "SHORT"
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

