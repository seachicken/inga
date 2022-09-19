(defpackage #:inga/test/main
  (:use #:cl
        #:fiveam
        #:inga/main))
(in-package #:inga/test/main)

;; NOTE: To run this test file, execute `(asdf:test-system :inga)' in your Lisp.

(def-suite main)
(in-suite main)

(defparameter *front-path*
  (truename (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/")))
(defparameter *back-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-boot-realworld-example-app/")))
(defparameter *build-path* (uiop:merge-pathnames* "test/fixtures/build/"))

(test analyze
  (multiple-value-bind (front back) (inga/main::start :front-path *front-path*)
    (is (equal
          '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10)))
          (inga/main::analyze front "4d33bd8")))
    (inga/main::stop front back)))

(test analyze-by-range-in-arrow-function
  (multiple-value-bind (front back) (inga/main::start :front-path *front-path*)
    (is (equal
          '(((:path . "src/App/TodoList/Item/index.tsx") (:line . 107) (:offset . 12)))
          (inga/main::analyze-by-range
            front
            '(("path" . "src/App/TodoList/Item/index.tsx") ("start" . 65) ("end" . 65)))))
    (inga/main::stop front back)))

(test analyze-by-range-in-function-declaration
  (multiple-value-bind (front back) (inga/main::start :front-path *front-path*)
    (is (equal
          '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10)))
          (inga/main::analyze-by-range
            front
            '(("path" . "src/App/NewTodoInput/index.tsx") ("start" . 14) ("end" . 14)))))
    (inga/main::stop front back)))

(test analyze-by-range-in-external-function
  (multiple-value-bind (front back) (inga/main::start
                                      :front-path *front-path*
                                      :exclude '("*.test.ts"))
    (is (equal
          '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10)))
          (inga/main::analyze-by-range
            front
            '(("path" . "src/functions.ts") ("start" . 2) ("end" . 2)))))
    (inga/main::stop front back)))

(test analyze-by-range-in-external-function-for-back
  (multiple-value-bind (front back) (inga/main::start
                                      :back-path *back-path*
                                      :exclude '("*Test.java"))
    (is (equal
          '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
             (:line . 48) (:offset . 3)))
          (inga/main::analyze-by-range
            back
            '(("path" . "src/main/java/io/spring/application/ArticleQueryService.java")
              ("start" . 105) ("end" . 105)))))
    (inga/main::stop front back)))

(test analyze-by-range-in-nested-external-function-for-back
  (multiple-value-bind (front back) (inga/main::start
                                      :back-path *back-path*
                                      :exclude '("*Test.java"))
    (is (equal
          '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
             (:line . 28) (:offset . 3))
            ((:path . "src/main/java/io/spring/graphql/ArticleMutation.java")
             (:line . 35) (:offset . 3)))
          (inga/main::analyze-by-range
            back
            '(("path" . "src/main/java/io/spring/core/article/Article.java")
              ("start" . 30) ("end" . 30)))))
    (inga/main::stop front back)))

(test analyze-by-range-in-interface-for-back
  (multiple-value-bind (front back) (inga/main::start
                                      :back-path *back-path*
                                      :exclude '("*Test.java"))
    (is (equal
          '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
             (:line . 28) (:offset . 3))
            ((:path . "src/main/java/io/spring/graphql/ArticleMutation.java")
             (:line . 35) (:offset . 3))
            ((:path . "src/main/java/io/spring/api/ArticleApi.java")
             (:line . 44) (:offset . 3))
            ((:path . "src/main/java/io/spring/graphql/ArticleMutation.java")
             (:line . 53) (:offset . 3)))
          (inga/main::analyze-by-range
            back
            '(("path" . "src/main/java/io/spring/core/article/ArticleRepository.java")
              ("start" . 7) ("end" . 7)))))
    (inga/main::stop front back)))

(test inject-mark
  (uiop:run-program (format nil "cp -r ~a ~a" *front-path* *build-path*))

  (inga/main::inject-mark
    *build-path*
    '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10))))
  (is (equal
        (uiop:read-file-string (uiop:merge-pathnames* "test/fixtures/index.tsx"))
        (uiop:read-file-string (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *build-path*))))
  
  (uiop:run-program (format nil "rm -r ~a" *build-path*)))

