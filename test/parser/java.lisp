(defpackage #:inga/test/parser/java
  (:use #:cl
        #:fiveam
        #:inga/parser))
(in-package #:inga/test/parser/java)

(def-suite parser/java)
(in-suite parser/java)

(defparameter *spring-boot-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-boot-realworld-example-app/")))

(test ignore-affected-pos-when-end-block
  (let ((parser (make-parser :java *spring-boot-path*)))
    (start-parser parser)
    (is (equal
          nil
          (let ((src-path "src/main/java/io/spring/core/article/Article.java"))
            (inga/parser/java::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              65))))
    (stop-parser parser)))

