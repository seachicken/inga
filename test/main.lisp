(defpackage #:inga/test/main
  (:use #:cl
        #:fiveam
        #:inga/main))
(in-package #:inga/test/main)

;; NOTE: To run this test file, execute `(asdf:test-system :inga)' in your Lisp.

(defparameter *build-path* (uiop:merge-pathnames* "test/fixtures/build/"))

(def-suite typescript)
(in-suite typescript)

(defparameter *front-path*
  (truename (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/")))
(defparameter *nestjs-path*
  (truename (uiop:merge-pathnames* "test/fixtures/nestjs-realworld-example-app-prisma/")))

(test analyze-by-range-for-react-components
  (let ((ctx (inga/main::start *front-path* '(:typescript) '("**/*.test.(ts|tsx)"))))
    (is (equal
          '(((:path . "src/App/NewTodoInput/index.tsx")
             (:name . "input")
             (:line . 34) (:offset . 10)))
          (mapcar (lambda (e) (cdr (assoc :entorypoint e)))
                  (inga/main::analyze-by-range
                    ctx
                    `((:path . "src/functions.ts")
                      ,(cons :start-offset
                             (inga/parser:convert-to-top-offset
                               *front-path*
                               "src/functions.ts"
                               '((:line . 2) (:offset . 0))))
                      ,(cons :end-offset
                             (inga/parser:convert-to-top-offset
                               *front-path*
                               "src/functions.ts"
                               '((:line . 2) (:offset . -1)))))))))
    (inga/main::stop ctx)))

(def-suite java)
(in-suite java)

(defparameter *back-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-boot-realworld-example-app/")))

(test analyze-by-range-for-entorypoints
  (let ((ctx (inga/main::start *back-path* '(:java) '("src/test/**"))))
    (is (equal
          '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
             (:name . "getArticles")
             (:line . 49) (:offset . 25)))
          (mapcar (lambda (e) (cdr (assoc :entorypoint e)))
                  (inga/main::analyze-by-range
                    ctx
                    `((:path . "src/main/java/io/spring/application/ArticleQueryService.java")
                      ,(cons :start-offset
                             (inga/parser:convert-to-top-offset
                               *back-path*
                               "src/main/java/io/spring/application/ArticleQueryService.java"
                               '((:line . 105) (:offset . 0))))
                      ,(cons :end-offset
                             (inga/parser:convert-to-top-offset
                               *back-path*
                               "src/main/java/io/spring/application/ArticleQueryService.java"
                               '((:line . 105) (:offset . -1)))))))))
    (inga/main::stop ctx)))

(test analyze-by-range-for-constraint-validator
  (if t
      (skip "TODO: implement")
      (let ((ctx (inga/main::start *back-path* '(:java) '("src/test/**"))))
        (is (equal
              '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
                 (:name . "createArticle") (:line . 29) (:offset . 25))
                ((:path . "src/main/java/io/spring/graphql/ArticleMutation.java")
                 (:name . "createArticle") (:line . 36) (:offset . 44)))
              (remove-duplicates
                (mapcar (lambda (e) (cdr (assoc :entorypoint e)))
                        (inga/main::analyze-by-range
                          ctx
                          '((:path . "src/main/java/io/spring/application/article/DuplicatedArticleValidator.java")
                            (:start . 16) (:end . 16))))
                :test #'equal)))
        (inga/main::stop ctx))))

(def-suite main)
(in-suite main)

(test get-analysis-kinds-for-java
  (is (equal
        '(:java)
        (inga/main::get-analysis-kinds
          '(((:path . "src/main/java/io/spring/application/article/NewArticleParam.java")
             (:start . 18) (:end . 18))
            ((:path . "src/main/java/io/spring/application/article/NewArticleParam.java")
             (:start . 20) (:end . 20)))))))

(test get-analysis-kinds-for-kotlin
  (is (equal
        '(:java)
        (inga/main::get-analysis-kinds
          '(((:path . "src/main/java/io/spring/application/article/NewArticleParam.kt")
             (:start . 18) (:end . 18))
            ((:path . "src/main/java/io/spring/application/article/NewArticleParam.kt")
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

