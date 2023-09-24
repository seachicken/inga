(defpackage #:inga/plugin/jvm-helper
  (:use #:cl)
  (:export #:find-base-path))
(in-package #:inga/plugin/jvm-helper)

(defun find-base-path (path)
  (when (equal path #p"/")
    (return-from find-base-path))

  (let ((parent-path (uiop:pathname-parent-directory-pathname path)))
    (loop for file in (uiop:directory-files parent-path)
          do
          (when (find (file-namestring file) '("pom.xml" "build.gradle") :test #'equal)
            (return-from find-base-path parent-path)))
    (find-base-path parent-path)))

