(defpackage #:inga/test/main
  (:use #:cl
        #:fiveam
        #:inga/main)
  (:import-from #:inga/traversal
                #:convert-to-top-offset))
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
  (let ((ctx (inga/main::start *front-path* '(:typescript) :exclude '("**/*.test.(ts|tsx)"))))
    (is (equal
          '(((:path . "src/App/NewTodoInput/index.tsx")
             (:name . "input")
             (:line . 34) (:offset . 10)))
          (remove-duplicates
            (mapcar (lambda (e) (cdr (assoc :entrypoint e)))
                    (inga/main::analyze-by-range
                      ctx
                      `((:path . "src/functions.ts")
                        ,(cons :start-offset
                               (convert-to-top-offset
                                 (merge-pathnames "src/functions.ts" *front-path*)
                                 '((:line . 2) (:offset . 0))))
                        ,(cons :end-offset
                               (convert-to-top-offset
                                 (merge-pathnames "src/functions.ts" *front-path*)
                                 '((:line . 2) (:offset . -1)))))))
            :test #'equal)))
    (inga/main::stop ctx)))

(def-suite java)
(in-suite java)

(defparameter *back-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-boot-realworld-example-app/")))
(defparameter *lightrun-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-tutorials/lightrun/")))

(test analyze-by-range-for-entrypoints
  (let ((ctx (inga/main::start *back-path* '(:java) :exclude '("src/test/**"))))
    (unwind-protect
      (is (equal
            '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
               (:name . "getArticles")
               (:line . 49) (:offset . 25)))
            (remove-duplicates
              (mapcar (lambda (e) (cdr (assoc :entrypoint e)))
                      (inga/main::analyze-by-range
                        ctx
                        `((:path . "src/main/java/io/spring/application/ArticleQueryService.java")
                          ,(cons :start-offset
                                 (convert-to-top-offset
                                   (merge-pathnames
                                     "src/main/java/io/spring/application/ArticleQueryService.java"
                                     *back-path*)
                                   '((:line . 105) (:offset . 0))))
                          ,(cons :end-offset
                                 (convert-to-top-offset
                                   (merge-pathnames
                                     "src/main/java/io/spring/application/ArticleQueryService.java"
                                     *back-path*)
                                   '((:line . 105) (:offset . -1)))))))
              :test #'equal)))
      (inga/main::stop ctx))))

(test analyze-by-range-for-micro-services
  (let ((ctx (inga/main::start *lightrun-path* '(:java))))
    (unwind-protect
      (is (equal
            '(((:type . "entrypoint")
               (:origin .
                ((:path . "users-service/src/main/java/com/baeldung/usersservice/service/UsersService.java")
                 (:name . "getUserById")
                 (:line . 34) (:offset . 23)))
               (:entrypoint .
                ((:path . "users-service/src/main/java/com/baeldung/usersservice/adapters/http/UsersController.java")
                 (:name . "getUser")
                 (:line . 38) (:offset . 25))))
              ((:type . "connection")
               (:origin .
                ((:path . "users-service/src/main/java/com/baeldung/usersservice/adapters/http/UsersController.java")
                 (:name . "getUser")
                 (:line . 38) (:offset . 25)))
               (:entrypoint .
                ((:path . "api-service/src/main/java/com/baeldung/apiservice/adapters/users/UserRepository.java")
                 (:name . "getUserById")
                 (:line . 18) (:offset . 17))))
              ((:type . "entrypoint")
               (:origin .
                ((:path . "api-service/src/main/java/com/baeldung/apiservice/adapters/users/UserRepository.java")
                 (:name . "getUserById")
                 (:line . 18) (:offset . 17)))
               (:entrypoint .
                ((:path . "api-service/src/main/java/com/baeldung/apiservice/adapters/http/TasksController.java")
                 (:name . "getTaskById")
                 (:line . 25) (:offset . 25)))))
            (remove-duplicates
              (inga/main::analyze-by-range
                ctx
                `((:path . "users-service/src/main/java/com/baeldung/usersservice/service/UsersService.java")
                  ,(cons :start-offset
                         (convert-to-top-offset
                           (merge-pathnames
                             "users-service/src/main/java/com/baeldung/usersservice/service/UsersService.java"
                             *lightrun-path*)
                           '((:line . 34) (:offset . 0))))
                  ,(cons :end-offset
                         (convert-to-top-offset
                           (merge-pathnames
                             "users-service/src/main/java/com/baeldung/usersservice/service/UsersService.java"
                             *lightrun-path*)
                           '((:line . 34) (:offset . -1))))))
              :test #'equal)))
      (inga/main::stop ctx))))

(test analyze-by-range-for-constraint-validator
  (if t
      (skip "TODO: implement")
      (let ((ctx (inga/main::start *back-path* '(:java) :exclude '("src/test/**"))))
        (is (equal
              '(((:path . "src/main/java/io/spring/api/ArticlesApi.java")
                 (:name . "createArticle") (:line . 29) (:offset . 25))
                ((:path . "src/main/java/io/spring/graphql/ArticleMutation.java")
                 (:name . "createArticle") (:line . 36) (:offset . 44)))
              (remove-duplicates
                (mapcar (lambda (e) (cdr (assoc :entrypoint e)))
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

