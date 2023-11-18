(defpackage #:inga/plugin/jvm-helper
  (:use #:cl)
  (:import-from #:inga/cache
                #:defunc)
  (:export #:find-base-path
           #:is-primitive-type))
(in-package #:inga/plugin/jvm-helper)

(defunc find-base-path (path)
  (when (equal path #p"/")
    (return-from find-base-path))

  (let ((parent-path (uiop:pathname-parent-directory-pathname path)))
    (loop for file in (uiop:directory-files parent-path)
          do
          (when (find (file-namestring file) '("pom.xml" "build.gradle") :test #'equal)
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


