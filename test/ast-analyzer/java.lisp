(defpackage #:inga/test/ast-analyzer/java
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer))
(in-package #:inga/test/ast-analyzer/java)

(def-suite java)
(in-suite java)

(defparameter *test-path* (merge-pathnames #p"test/"))
(defparameter *spring-boot-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-boot-realworld-example-app/")))
(defparameter *cache* (inga/cache:make-cache 100))

(test find-definitions-for-constructor
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "fixtures/java/ConstructorDefinition.java")
             (:name . "ConstructorDefinition")
             (:fq-name . "fixtures.java.ConstructorDefinition.ConstructorDefinition")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/java/ConstructorDefinition.java"
                      '((:line . 4) (:offset . 12))))))
          (find-definitions
            ast-analyzer
            `((:path . "fixtures/java/ConstructorDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *test-path* "fixtures/java/ConstructorDefinition.java"
                       '((:line . 4) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *test-path* "fixtures/java/ConstructorDefinition.java"
                       '((:line . 4) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-definitions-for-method
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "fixtures/java/MethodDefinition.java")
             (:name . "method")
             (:fq-name . "fixtures.java.MethodDefinition.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                       *test-path* "fixtures/java/MethodDefinition.java"
                      '((:line . 7) (:offset . 17))))))
          (find-definitions
            ast-analyzer
            `((:path . "fixtures/java/MethodDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *test-path* "fixtures/java/MethodDefinition.java"
                       '((:line . 7) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *test-path* "fixtures/java/MethodDefinition.java"
                       '((:line . 7) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-definitions-for-interface
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "fixtures/java/InterfaceDefinition.java")
             (:name . "method")
             (:fq-name . "fixtures.java.InterfaceDefinition.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/java/InterfaceDefinition.java"
                      '((:line . 6) (:offset . 10))))))
          (find-definitions
            ast-analyzer
            `((:path . "fixtures/java/InterfaceDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *test-path* "fixtures/java/InterfaceDefinition.java"
                       '((:line . 6) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *test-path* "fixtures/java/InterfaceDefinition.java"
                       '((:line . 6) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-definitions-for-instance-variable-annotation
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "fixtures/java/InstanceVariableAnnotationDefinition.java")
             (:name . "variable")
             (:fq-name . "fixtures.java.InstanceVariableAnnotationDefinition.variable")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/java/InstanceVariableAnnotationDefinition.java"
                      '((:line . 7) (:offset . 19))))))
          (find-definitions
            ast-analyzer
            `((:path . "fixtures/java/InstanceVariableAnnotationDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *test-path* "fixtures/java/InstanceVariableAnnotationDefinition.java"
                       '((:line . 6) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *test-path* "fixtures/java/InstanceVariableAnnotationDefinition.java"
                       '((:line . 6) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

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
  (let ((ast-analyzer (make-ast-analyzer :java *spring-boot-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:type . :rest-server)
             (:path . "/articles")
             (:name . "GET")
             (:file-pos .
              ((:path . "src/main/java/io/spring/api/ArticlesApi.java")
               (:name . "getArticles")
               (:fq-name . "io.spring.api.ArticlesApi.getArticles-INT-INT-String-String-String-User")
               ,(cons :top-offset
                    (convert-to-top-offset
                      *spring-boot-path*
                      "src/main/java/io/spring/api/ArticlesApi.java"
                      '((:line . 49) (:offset . 25))))))))
          (find-definitions
            ast-analyzer
            `((:path . "src/main/java/io/spring/api/ArticlesApi.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *spring-boot-path*
                       "src/main/java/io/spring/api/ArticlesApi.java"
                       '((:line . 56) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *spring-boot-path*
                       "src/main/java/io/spring/api/ArticlesApi.java"
                       '((:line . 56) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

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
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "fixtures/java/NewClassReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/java/NewClassReference.java"
                      '((:line . 7) (:offset . 9))))))
          (find-references ast-analyzer
                           `((:path . "fixtures/java/NewClassHelper.java")
                             (:name . "method")
                             (:fq-name . "fixtures.java.NewClassHelper.method")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-for-constructor
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "fixtures/java/ConstructorReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/java/ConstructorReference.java"
                      '((:line . 7) (:offset . 9))))))
          (find-references ast-analyzer
                           `((:path . "fixtures/java/ConstructorHelper.java")
                             (:name . "ConstructorHelper")
                             (:fq-name . "fixtures.java.ConstructorHelper.ConstructorHelper-INT")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-for-private-method
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "fixtures/java/PrivateMethodReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/java/PrivateMethodReference.java"
                      '((:line . 5) (:offset . 9))))))
          (find-references ast-analyzer
                           `((:path . "fixtures/java/PrivateMethodReference.java")
                             (:name . "method2")
                             (:fq-name . "fixtures.java.PrivateMethodReference.method2")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-for-rest-client
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "fixtures/java/client/ClientRestTemplate.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/java/client/ClientRestTemplate.java"
                      '((:line . 15) (:offset . 16)))))
            ((:path . "fixtures/java/client/ClientRestTemplate.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/java/client/ClientRestTemplate.java"
                      '((:line . 23) (:offset . 16))))))
          (find-references ast-analyzer
                           `((:type . :rest-server)
                             (:path . "/path")
                             (:name . "GET")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-for-kotlin-class
  (let ((ast-analyzer (make-ast-analyzer :java *test-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "fixtures/java/KotlinReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *test-path* "fixtures/java/KotlinReference.java"
                      '((:line . 9) (:offset . 9))))))
          (find-references ast-analyzer
                           '((:path . "fixtures/kotlin/JavaReference.kt")
                             (:name . "method")
                             (:fq-name . "fixtures.kotlin.JavaReference.method")))))
    (stop-ast-analyzer ast-analyzer)))

