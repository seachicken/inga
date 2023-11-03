(defpackage #:inga/test/ast-analyzer/java
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer))
(in-package #:inga/test/ast-analyzer/java)

(def-suite java)
(in-suite java)

(defparameter *java-path* (merge-pathnames #p"test/fixtures/java/"))
(defparameter *spring-boot-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-boot-realworld-example-app/")))
(defparameter *lightrun-path* (merge-pathnames "test/fixtures/spring-tutorials/lightrun/"))

(test find-definitions-for-constructor
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:type . :module-public)
             (:path . "p1/ConstructorDefinition.java")
             (:name . "ConstructorDefinition")
             (:fq-name . "p1.ConstructorDefinition.ConstructorDefinition")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/ConstructorDefinition.java" *java-path*)
                      '((:line . 4) (:offset . 12))))))
          (find-definitions
            `((:path . "p1/ConstructorDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/ConstructorDefinition.java" *java-path*)
                       '((:line . 4) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/ConstructorDefinition.java" *java-path*)
                       '((:line . 4) (:offset . -1))))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-definitions-for-method
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:type . :module-public)
             (:path . "p1/MethodDefinition.java")
             (:name . "method")
             (:fq-name . "p1.MethodDefinition.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/MethodDefinition.java" *java-path*)
                      '((:line . 7) (:offset . 17))))))
          (find-definitions
            `((:path . "p1/MethodDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/MethodDefinition.java" *java-path*)
                       '((:line . 7) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/MethodDefinition.java" *java-path*)
                       '((:line . 7) (:offset . -1))))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-definitions-for-interface
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:type . :module-default)
             (:path . "p1/InterfaceDefinition.java")
             (:name . "method")
             (:fq-name . "p1.InterfaceDefinition.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/InterfaceDefinition.java" *java-path*)
                      '((:line . 6) (:offset . 10))))))
          (find-definitions
            `((:path . "p1/InterfaceDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/InterfaceDefinition.java" *java-path*)
                       '((:line . 6) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/InterfaceDefinition.java" *java-path*)
                       '((:line . 6) (:offset . -1))))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-definitions-for-instance-variable-annotation
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:type . :module-public)
             (:path . "p1/InstanceVariableAnnotationDefinition.java")
             (:name . "variable")
             (:fq-name . "p1.InstanceVariableAnnotationDefinition.variable")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/InstanceVariableAnnotationDefinition.java" *java-path*)
                      '((:line . 7) (:offset . 19))))))
          (find-definitions
            `((:path . "p1/InstanceVariableAnnotationDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/InstanceVariableAnnotationDefinition.java" *java-path*)
                       '((:line . 6) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/InstanceVariableAnnotationDefinition.java" *java-path*)
                       '((:line . 6) (:offset . -1))))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-definitions-for-generic-type
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:type . :module-public)
             (:path . "p1/GenericTypeDefinition.java")
             (:name . "GenericTypeDefinition")
             (:fq-name . "p1.GenericTypeDefinition.GenericTypeDefinition-java.util.List")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/GenericTypeDefinition.java" *java-path*)
                      '((:line . 6) (:offset . 12))))))
          (find-definitions
            `((:path . "p1/GenericTypeDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/GenericTypeDefinition.java" *java-path*)
                       '((:line . 6) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames "p1/GenericTypeDefinition.java" *java-path*)
                       '((:line . 6) (:offset . -1))))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

;; class DuplicatedArticleValidator
;;                                    ↓[out]
;;     implements ConstraintValidator<DuplicatedArticleConstraint, String> {
;;   @Override
;;   public boolean isValid(String value, ConstraintValidatorContext context) {
;;     return true; ←[in]
;;   }
;; }
(test find-definitions-for-constraint-validator
  (if t
      (skip "TODO: implement")
      (let ((ast-analyzer (make-ast-analyzer :java *spring-boot-path* *cache*)))
        (start-ast-analyzer ast-analyzer nil nil)
        (is (equal
              '(((:path . "src/main/java/io/spring/application/article/DuplicatedArticleValidator.java")
                 (:name . "DuplicatedArticleConstraint") (:line . 10) (:offset . 36)))
              (find-definitions
                ast-analyzer
                '((:path . "src/main/java/io/spring/application/article/DuplicatedArticleValidator.java")
                  (:start . 16) (:end . 16)))))
        (stop-ast-analyzer ast-analyzer))))

(test find-definitions-for-spring-rest-controller
  (inga/plugin/jvm-dependency-loader:start *spring-boot-path*)
  (inga/plugin/spring-property-loader:start *spring-boot-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *spring-boot-path*)
            (start-ast-analyzer :kotlin nil *spring-boot-path*))))
    (create-indexes *spring-boot-path* :include inga/main::*include-java*)
    (is (equal
          `(((:type . :rest-server)
             (:host . "8080")
             (:name . "GET")
             (:path . "/articles")
             (:file-pos .
              ((:type . :module-public)
               (:path . "src/main/java/io/spring/api/ArticlesApi.java")
               (:name . "getArticles")
               (:fq-name . "io.spring.api.ArticlesApi.getArticles-INT-INT-java.lang.String-java.lang.String-java.lang.String-io.spring.core.user.User")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "src/main/java/io/spring/api/ArticlesApi.java"
                          *spring-boot-path*)
                        '((:line . 49) (:offset . 25))))))))
          (find-definitions
            `((:path . "src/main/java/io/spring/api/ArticlesApi.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames
                         "src/main/java/io/spring/api/ArticlesApi.java"
                         *spring-boot-path*)
                       '((:line . 56) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames
                         "src/main/java/io/spring/api/ArticlesApi.java"
                         *spring-boot-path*)
                       '((:line . 56) (:offset . -1))))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))
    (inga/plugin/spring-property-loader:stop)
    (inga/plugin/jvm-dependency-loader:stop)))

(test find-definitions-for-spring-rest-get-method
  (inga/plugin/spring-property-loader:start *java-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:type . :rest-server)
             (:host . "8080")
             (:name . "GET")
             (:path . "/{string}")
             (:file-pos .
              ((:type . :module-public)
               (:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
               (:name . "get")
               (:fq-name . "p1.RestControllerDefinition.get-java.lang.String")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                        '((:line . 15) (:offset . 17))))))))
          (find-definitions
            `((:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames
                         "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                       '((:line . 15) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames
                         "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                       '((:line . 15) (:offset . -1))))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a)))
    (inga/plugin/spring-property-loader:stop))

(test find-definitions-for-spring-rest-post-method
  (inga/plugin/spring-property-loader:start *java-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:type . :rest-server)
             (:host . "8080")
             (:name . "POST")
             (:path . "/")
             (:file-pos .
              ((:type . :module-public)
               (:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
               (:name . "create")
               (:fq-name . "p1.RestControllerDefinition.create")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                        '((:line . 19) (:offset . 17))))))))
          (find-definitions
            `((:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames
                         "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                       '((:line . 19) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames
                         "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                       '((:line . 19) (:offset . -1))))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a)))
    (inga/plugin/spring-property-loader:stop))

(test find-definitions-for-spring-rest-put-method
  (inga/plugin/spring-property-loader:start *java-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:type . :rest-server)
             (:host . "8080")
             (:name . "PUT")
             (:path . "/{string}")
             (:file-pos .
              ((:type . :module-public)
               (:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
               (:name . "update")
               (:fq-name . "p1.RestControllerDefinition.update-java.lang.String")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                        '((:line . 23) (:offset . 17))))))))
          (find-definitions
            `((:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames
                         "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                       '((:line . 23) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames
                         "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                       '((:line . 23) (:offset . -1))))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a)))
    (inga/plugin/spring-property-loader:stop))

(test find-definitions-for-spring-rest-delete-method
  (inga/plugin/spring-property-loader:start *java-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:type . :rest-server)
             (:host . "8080")
             (:name . "DELETE")
             (:path . "/{string}")
             (:file-pos .
              ((:type . :module-public)
               (:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
               (:name . "delete")
               (:fq-name . "p1.RestControllerDefinition.delete-java.lang.String")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                        '((:line . 27) (:offset . 17))))))))
          (find-definitions
            `((:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames
                         "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                       '((:line . 27) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames
                         "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                       '((:line . 27) (:offset . -1))))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a)))
    (inga/plugin/spring-property-loader:stop))

;; public class Article {
;;   public void update(String title, String description, String body) {
;;   } ←[in]
;; }
(test ignore-affected-poss-when-end-block
  (if t
      (skip "TODO: implement")
      (let ((ast-analyzer (make-ast-analyzer :java *spring-boot-path* *cache*)))
        (start-ast-analyzer ast-analyzer nil nil)
        (is (equal
              nil
              (find-definitions
                ast-analyzer
                '((:path . "src/main/java/io/spring/core/article/Article.java")
                (:start . 65) (:end . 65)))))
        (stop-ast-analyzer ast-analyzer))))

(test find-references-for-new-class
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:path . "p1/NewClassReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/NewClassReference.java" *java-path*)
                      '((:line . 7) (:offset . 9))))))
          (find-references
            `((:path . "p1/NewClassHelper.java")
              (:name . "method")
              (:fq-name . "p1.NewClassHelper.method")))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-references-for-constructor
  (inga/plugin/jvm-dependency-loader:start *spring-boot-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:path . "p1/ConstructorReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/ConstructorReference.java" *java-path*)
                      '((:line . 7) (:offset . 9))))))
          (find-references
            `((:path . "p1/ConstructorHelper.java")
              (:name . "ConstructorHelper")
              (:fq-name . "p1.ConstructorHelper.ConstructorHelper-INT")))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))
    (inga/plugin/jvm-dependency-loader:stop)))

(test find-references-for-private-method
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:path . "p1/PrivateMethodReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/PrivateMethodReference.java" *java-path*)
                      '((:line . 5) (:offset . 9))))))
          (find-references
            `((:path . "p1/PrivateMethodReference.java")
              (:name . "method2")
              (:fq-name . "p1.PrivateMethodReference.method2")))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-references-with-sub-class-args
  (inga/plugin/jvm-dependency-loader:start *spring-boot-path*)
  (inga/plugin/spring-property-loader:start *spring-boot-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *spring-boot-path*)
            (start-ast-analyzer :kotlin nil *spring-boot-path*))))
    (create-indexes *spring-boot-path* :include inga/main::*include-java*)
    (is (equal
          `(((:path . "src/main/java/io/spring/application/ArticleQueryService.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames
                        "src/main/java/io/spring/application/ArticleQueryService.java"
                        *spring-boot-path*)
                      '((:line . 63) (:offset . 14)))))
            ((:path . "src/main/java/io/spring/application/ArticleQueryService.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames
                        "src/main/java/io/spring/application/ArticleQueryService.java"
                        *spring-boot-path*)
                      '((:line . 84) (:offset . 14)))))
            ((:path . "src/main/java/io/spring/application/CommentQueryService.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames
                        "src/main/java/io/spring/application/CommentQueryService.java"
                        *spring-boot-path*)
                      '((:line . 60) (:offset . 14))))))
          (find-references
            `((:path . "src/main/java/io/spring/application/CursorPager.java")
              (:name . "CursorPager")
              (:fq-name . "io.spring.application.CursorPager.CursorPager-java.util.List-io.spring.application.CursorPager.Direction-BOOLEAN")))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))
    (inga/plugin/spring-property-loader:stop)
    (inga/plugin/jvm-dependency-loader:stop)))

(test find-references-for-rest-client-get-method
  (inga/plugin/jvm-dependency-loader:start *java-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:path . "p1/client/ClientRestTemplate.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/client/ClientRestTemplate.java" *java-path*)
                      '((:line . 16) (:offset . 16)))))
            ((:path . "p1/client/ClientRestTemplate.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/client/ClientRestTemplate.java" *java-path*)
                      '((:line . 24) (:offset . 16))))))
          (find-references
            `((:type . :rest-server)
              (:host . "8080")
              (:path . "/path")
              (:name . "GET")))))
    (clean-indexes)
    (inga/plugin/jvm-dependency-loader:stop)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-references-for-rest-client-post-method
  (inga/plugin/jvm-dependency-loader:start *java-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:path . "p1/client/ClientRestTemplate.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/client/ClientRestTemplate.java" *java-path*)
                      '((:line . 28) (:offset . 16))))))
          (find-references
            `((:type . :rest-server)
              (:host . "8080")
              (:path . "/path")
              (:name . "POST")))))
    (clean-indexes)
    (inga/plugin/jvm-dependency-loader:stop)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-references-for-kotlin-class
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          `(((:path . "p1/KotlinReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/KotlinReference.java" *java-path*)
                      '((:line . 9) (:offset . 9))))))
          (find-references
            '((:path . "p1/JavaReference.kt")
              (:name . "method")
              (:fq-name . "p1.JavaReference.method")))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test get-scoped-index-paths-with-module-private
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *lightrun-path*)
            (start-ast-analyzer :kotlin nil *lightrun-path*))))
    (create-indexes *lightrun-path* :include inga/main::*include-java*)
    (is (equal
          `(,(merge-pathnames
               (get-index-path "api-service/src/main/java/com/baeldung/apiservice/adapters/http/TasksController.java")
               *lightrun-path*))
          (inga/ast-analyzer/base::get-scoped-index-paths
            '((:type . :module-private)
              (:path . "api-service/src/main/java/com/baeldung/apiservice/adapters/http/TasksController.java")
              (:fq-name . "com.baeldung.apiservice.adapters.http.TasksController.getUser-java.lang.String")
              ))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test get-scoped-index-paths-with-module-public
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *lightrun-path*)
            (start-ast-analyzer :kotlin nil *lightrun-path*))))
    (create-indexes *lightrun-path* :include inga/main::*include-java*)
    (is (null
          (find-if-not
            (lambda (p) (uiop:string-prefix-p "api-service" p))
            (mapcar (lambda (p) (enough-namestring p inga/ast-analyzer/base::*index-path*))
                    (inga/ast-analyzer/base::get-scoped-index-paths
                      '((:type . :module-public)
                        (:path . "api-service/src/main/java/com/baeldung/apiservice/adapters/users/UserRepository.java")
                        (:fq-name . "com.baeldung.apiservice.adapters.users.UserRepository.getUserById-java.lang.String")
                        ))))))
    (clean-indexes)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test matches-signature
  (inga/plugin/jvm-dependency-loader:start *java-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (eq
          t
          (matches-signature "p2.ApiSignature-p2.ChildClass" "p2.ApiSignature-p2.ParentClass")))
    (clean-indexes)
    (inga/plugin/jvm-dependency-loader:stop)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test matches-signature-with-sub-class
  (inga/plugin/jvm-dependency-loader:start *spring-boot-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *spring-boot-path*)
            (start-ast-analyzer :kotlin nil *spring-boot-path*))))
    (create-indexes *spring-boot-path* :include inga/main::*include-java*)
    (is (eq
          t
          (matches-signature
            "io.spring.application.CursorPager.CursorPager-java.util.ArrayList-io.spring.application.CursorPager.Direction-BOOLEAN"
            "io.spring.application.CursorPager.CursorPager-java.util.List-io.spring.application.CursorPager.Direction-BOOLEAN")))
    (clean-indexes)
    (inga/plugin/jvm-dependency-loader:stop)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-class-hierarchy-with-standard-class
  (inga/plugin/jvm-dependency-loader:start *lightrun-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *lightrun-path*)
            (start-ast-analyzer :kotlin nil *lightrun-path*))))
    (create-indexes *lightrun-path* :include inga/main::*include-java*)
    (is (equal
          '("java.io.Serializable"
            "java.lang.Comparable"
            "java.lang.CharSequence"
            "java.lang.constant.Constable"
            "java.lang.constant.ConstantDesc"
            "java.lang.Object"
            "java.lang.String")
          (find-class-hierarchy "java.lang.String")))
    (clean-indexes)
    (inga/plugin/jvm-dependency-loader:stop)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

(test find-class-hierarchy-with-app-class
  (inga/plugin/jvm-dependency-loader:start *java-path*)
  (let ((ast-analyzers
          (list
            (start-ast-analyzer :java nil *java-path*)
            (start-ast-analyzer :kotlin nil *java-path*))))
    (create-indexes *java-path* :include inga/main::*include-java*)
    (is (equal
          '("java.lang.Object"
            "p2.ParentClass"
            "p2.ChildClass")
          (find-class-hierarchy "p2.ChildClass")))
    (clean-indexes)
    (inga/plugin/jvm-dependency-loader:stop)
    (loop for a in ast-analyzers do (stop-ast-analyzer a))))

