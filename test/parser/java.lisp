(defpackage #:inga/test/parser/java
  (:use #:cl
        #:fiveam
        #:inga/parser))
(in-package #:inga/test/parser/java)

(def-suite java)
(in-suite java)

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

(test count-combinations
  (let ((parser (make-parser :java *spring-boot-path*)))
    (start-parser parser)
    (is (equal
          2
          (let ((src-path "src/main/java/io/spring/core/article/Article.java"))
            (inga/parser/typescript::count-combinations
              parser
              src-path
              (exec-parser parser src-path)
              '(53 54)))))
    (stop-parser parser)))

