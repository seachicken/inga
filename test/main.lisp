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
(defparameter *nestjs-path*
  (truename (uiop:merge-pathnames* "test/fixtures/nestjs-realworld-example-app-prisma/")))
(defparameter *build-path* (uiop:merge-pathnames* "test/fixtures/build/"))

(test analyze-by-range-in-arrow-function
  (let ((ctx (inga/main::start *front-path* '(:typescript))))
    (is (equal
          '(((:path . "src/App/TodoList/Item/index.tsx")
             (:name . "input") (:line . 107) (:offset . 12)))
          (mapcar (lambda (e) (cdr (assoc :entorypoint e)))
                  (inga/main::analyze-by-range
                    ctx
                    '((:path . "src/App/TodoList/Item/index.tsx")
                      (:start . 65) (:end . 65))))))
    (inga/main::stop ctx)))

(test analyze-by-range-in-function-declaration
  (let ((ctx (inga/main::start *front-path* '(:typescript))))
    (is (equal
          '(((:path . "src/App/NewTodoInput/index.tsx")
             (:name . "input") (:line . 34) (:offset . 10)))
          (mapcar (lambda (e) (cdr (assoc :entorypoint e)))
                  (inga/main::analyze-by-range
                    ctx
                    '((:path . "src/App/NewTodoInput/index.tsx")
                      (:start . 14) (:end . 14))))))
    (inga/main::stop ctx)))

(test analyze-by-range-in-external-function
  (let ((ctx (inga/main::start *front-path* '(:typescript) '("*.test.ts"))))
    (is (equal
          '(((:path . "src/App/NewTodoInput/index.tsx")
             (:name . "input") (:line . 34) (:offset . 10)))
          (mapcar (lambda (e) (cdr (assoc :entorypoint e)))
                  (inga/main::analyze-by-range
                    ctx
                    '((:path . "src/functions.ts")
                      (:start . 2) (:end . 2))))))
    (inga/main::stop ctx)))

(test analyze-by-range-in-external-function-for-back
  (let ((ctx (inga/main::start *back-path* '(:java) '("src/test/**"))))
    (is (equal
          '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
             (:name . "getArticles") (:line . 49) (:offset . 25)))
          (mapcar (lambda (e) (cdr (assoc :entorypoint e)))
                  (inga/main::analyze-by-range
                    ctx
                    '((:path . "src/main/java/io/spring/application/ArticleQueryService.java")
                      (:start . 105) (:end . 105))))))
    (inga/main::stop ctx)))

(test analyze-by-range-in-nested-external-function-for-back
  (let ((ctx (inga/main::start *back-path* '(:java) '("src/test/**"))))
    (is (equal
          '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
             (:name . "createArticle") (:line . 29) (:offset . 25))
            ((:path . "src/main/java/io/spring/graphql/ArticleMutation.java")
             (:name . "createArticle") (:line . 36) (:offset . 44)))
          (mapcar (lambda (e) (cdr (assoc :entorypoint e)))
                  (inga/main::analyze-by-range
                    ctx
                    '((:path . "src/main/java/io/spring/core/article/Article.java")
                      (:start . 30) (:end . 30))))))
    (inga/main::stop ctx)))

(test analyze-by-range-in-interface-for-back
  (let ((ctx (inga/main::start *back-path* '(:java) '("src/test/**"))))
    (is (equal
          '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
             (:name . "createArticle") (:line . 29) (:offset . 25))
            ((:path . "src/main/java/io/spring/graphql/ArticleMutation.java")
             (:name . "createArticle") (:line . 36) (:offset . 44))
            ((:path . "src/main/java/io/spring/api/ArticleApi.java")
             (:name . "updateArticle") (:line . 45) (:offset . 28))
            ((:path . "src/main/java/io/spring/graphql/ArticleMutation.java")
             (:name . "updateArticle") (:line . 54) (:offset . 44)))
          (mapcar (lambda (e) (cdr (assoc :entorypoint e)))
                  (inga/main::analyze-by-range
                    ctx
                    '((:path . "src/main/java/io/spring/core/article/ArticleRepository.java")
                      (:start . 7) (:end . 8))))))
    (inga/main::stop ctx)))

(test analyze-by-range-in-field-annotation
  (let ((ctx (inga/main::start *back-path* '(:java) '("src/test/**"))))
    (is (equal
          '(((:path . "src/main/java/io/spring/graphql/ArticleMutation.java")
             (:name . "createArticle") (:line . 36) (:offset . 44))
            ((:path . "src/main/java/io/spring/api/ArticlesApi.java")
             (:name . "createArticle") (:line . 29) (:offset . 25))
            ((:path . "src/main/java/io/spring/graphql/ArticleMutation.java")
             (:name . "createArticle") (:line . 36) (:offset . 44)))
          (mapcar (lambda (e) (cdr (assoc :entorypoint e)))
                  (inga/main::analyze-by-range
                    ctx
                    '((:path . "src/main/java/io/spring/application/article/NewArticleParam.java")
                      (:start . 18) (:end . 18))))))
    (inga/main::stop ctx)))

(test analyze-by-range-in-method
  (let ((ctx (inga/main::start *nestjs-path* '(:typescript))))
    (is (equal
          '(((:path . "src/article/article.controller.ts")
             (:name . "findAll") (:line . 24) (:offset . 9)))
          (mapcar (lambda (e) (cdr (assoc :entorypoint e)))
                  (inga/main::analyze-by-range
                    ctx
                    '((:path . "src/article/article.service.ts")
                      (:start . 47) (:end . 47))))))
    (inga/main::stop ctx)))

(test get-analysis-kinds
  (is (equal
        '(:java)
        (inga/main::get-analysis-kinds
          '(((:path . "src/main/java/io/spring/application/article/NewArticleParam.java")
             (:start . 18) (:end . 18))
            ((:path . "src/main/java/io/spring/application/article/NewArticleParam.java")
             (:start . 20) (:end . 20)))))))

(test filter-active-context-with-no-env
  (is (equal
        '(:java)
        (inga/main::filter-active-context
          '(:java) nil))))

(test filter-active-context-with-java-env
  (is (equal
        '(:java)
        (inga/main::filter-active-context
          '(:typescript :java) '(:java)))))

(test throw-error-when-option-does-not-exist
  (signals inga/main::inga-error-option-not-found
    (inga/main::parse-argv '("--not-exist"))))

(test inject-mark
  (uiop:run-program (format nil "cp -r ~a ~a" *front-path* *build-path*))

  (inga/main::inject-mark
    *build-path*
    '(((:path . "src/App/NewTodoInput/index.tsx")
       (:line . 34) (:offset . 10))))
  (is (equal
        (uiop:read-file-string (uiop:merge-pathnames* "test/fixtures/index.tsx"))
        (uiop:read-file-string (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *build-path*))))
  
  (uiop:run-program (format nil "rm -r ~a" *build-path*)))

