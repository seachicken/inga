(defpackage #:inga/test/helper
  (:use #:cl)
  (:export #:compile-gradle
           #:compile-maven))
(in-package #:inga/test/helper)

(defun compile-gradle (path)
  (let ((bash (uiop:launch-program "bash" :input :stream :output :stream)))
    (write-line (format nil "(cd ~a && ./gradlew compileJava)" path) (uiop:process-info-input bash))
    (force-output (uiop:process-info-input bash))))

(defun compile-maven (path)
  (let ((bash (uiop:launch-program "bash" :input :stream :output :stream)))
    (write-line (format nil "(cd ~a && mvn compile)" path) (uiop:process-info-input bash))
    (force-output (uiop:process-info-input bash))))

