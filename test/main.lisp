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
  (inga:start :front-path *front-path*)
  (is (equal
        '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10)))
        (inga/main::analyze-for-front *front-path* "a690a51" "4d33bd8")))
  (inga:stop :front-path *front-path*))

(test analyze-by-range-in-arrow-function
  (inga:start :front-path *front-path*)
  (is (equal
        '(((:path . "src/App/TodoList/Item/index.tsx") (:line . 107) (:offset . 12)))
        (inga/main::analyze-by-range
          *front-path*
          '(("path" . "src/App/TodoList/Item/index.tsx") ("start" . 65) ("end" . 65)))))
  (inga:stop :front-path *front-path*))

(test analyze-by-range-in-function-declaration
  (inga:start :front-path *front-path*)
  (is (equal
        '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10)))
        (inga/main::analyze-by-range
          *front-path*
          '(("path" . "src/App/NewTodoInput/index.tsx") ("start" . 14) ("end" . 14)))))
  (inga:stop :front-path *front-path*))

(test analyze-by-range-in-external-function
  (inga:start :front-path *front-path*)
  (is (equal
        '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10)))
        (inga/main::analyze-by-range
          *front-path*
          '(("path" . "src/functions.ts") ("start" . 2) ("end" . 2))   
          '("*.test.ts"))))
  (inga:stop :front-path *front-path*))

(test analyze-by-range-in-external-function-for-back
  (inga:start :back-path *back-path*)
  (is (equal
        '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
           (:line . 48) (:offset . 3)))
        (inga/main::analyze-by-range-for-back
          *back-path*
          '(("path" . "src/main/java/io/spring/application/ArticleQueryService.java")
            ("start" . 105) ("end" . 105))
          '("*Test.java"))))
  (inga:stop :back-path *back-path*))

(test analyze-by-range-in-nested-external-function-for-back
  (inga:start :back-path *back-path*)
  (is (equal
        '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
           (:line . 28) (:offset . 3)))
        (inga/main::analyze-by-range-for-back
          *back-path*
          '(("path" . "src/main/java/io/spring/core/article/Article.java")
            ("start" . 30) ("end" . 30))
          '("*Test.java"))))
  (inga:stop :back-path *back-path*))

(test analyze-by-range-in-interface-for-back
  (inga:start :back-path *back-path*)
  (is (equal
        '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
           (:line . 28) (:offset . 3)))
        (inga/main::analyze-by-range-for-back
          *back-path*
          '(("path" . "src/main/java/io/spring/core/article/ArticleRepository.java")
            ("start" . 7) ("end" . 7))
          '("*Test.java"))))
  (inga:stop :back-path *back-path*))

(test find-components
  (inga:start :front-path *front-path*)
  (is (equal
        '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10)))
        (inga:find-components
          *front-path*
          (truename (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *front-path*))
          '((:path . "src/App/NewTodoInput/index.tsx") (:line . 12) (:offset . 12))
          (inga/main::make-queue))))
  (inga:stop :back-path *back-path*))

(test find-components-with-exclude
  (inga:start :front-path *front-path*)
  (is (equal
        '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10))
          ((:path . "src/App/index.tsx") (:line . 34) (:offset . 10))) 
        (inga:find-components
          *front-path*
          (truename (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *front-path*))
          '((:path . "src/App/NewTodoInput/index.tsx") (:line . 7) (:offset . 7))
          (inga/main::make-queue)
          '("*.test.tsx"))))
  (inga:stop :back-path (truename *back-path*)))

(test inject-mark
  (uiop:run-program (format nil "cp -r ~a ~a" *front-path* *build-path*))

  (inga:inject-mark
    *build-path*
    '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10))))
  (is (equal
        (uiop:read-file-string (uiop:merge-pathnames* "test/fixtures/index.tsx"))
        (uiop:read-file-string (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *build-path*))))
  
  (uiop:run-program (format nil "rm -r ~a" *build-path*)))

