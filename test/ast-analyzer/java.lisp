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
(defparameter *cache* (inga/cache:make-cache 100))

(test find-definitions-for-constructor
  (let ((ast-analyzer (make-ast-analyzer :java *java-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "p1/ConstructorDefinition.java")
             (:name . "ConstructorDefinition")
             (:fq-name . "p1.ConstructorDefinition.ConstructorDefinition")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *java-path* "p1/ConstructorDefinition.java"
                      '((:line . 4) (:offset . 12))))))
          (find-definitions
            ast-analyzer
            `((:path . "p1/ConstructorDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *java-path* "p1/ConstructorDefinition.java"
                       '((:line . 4) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *java-path* "p1/ConstructorDefinition.java"
                       '((:line . 4) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-definitions-for-method
  (let ((ast-analyzer (make-ast-analyzer :java *java-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "p1/MethodDefinition.java")
             (:name . "method")
             (:fq-name . "p1.MethodDefinition.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                       *java-path* "p1/MethodDefinition.java"
                      '((:line . 7) (:offset . 17))))))
          (find-definitions
            ast-analyzer
            `((:path . "p1/MethodDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *java-path* "p1/MethodDefinition.java"
                       '((:line . 7) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *java-path* "p1/MethodDefinition.java"
                       '((:line . 7) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-definitions-for-interface
  (let ((ast-analyzer (make-ast-analyzer :java *java-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "p1/InterfaceDefinition.java")
             (:name . "method")
             (:fq-name . "p1.InterfaceDefinition.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *java-path* "p1/InterfaceDefinition.java"
                      '((:line . 6) (:offset . 10))))))
          (find-definitions
            ast-analyzer
            `((:path . "p1/InterfaceDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *java-path* "p1/InterfaceDefinition.java"
                       '((:line . 6) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *java-path* "p1/InterfaceDefinition.java"
                       '((:line . 6) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-definitions-for-instance-variable-annotation
  (let ((ast-analyzer (make-ast-analyzer :java *java-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "p1/InstanceVariableAnnotationDefinition.java")
             (:name . "variable")
             (:fq-name . "p1.InstanceVariableAnnotationDefinition.variable")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *java-path* "p1/InstanceVariableAnnotationDefinition.java"
                      '((:line . 7) (:offset . 19))))))
          (find-definitions
            ast-analyzer
            `((:path . "p1/InstanceVariableAnnotationDefinition.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *java-path* "p1/InstanceVariableAnnotationDefinition.java"
                       '((:line . 6) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *java-path* "p1/InstanceVariableAnnotationDefinition.java"
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
               (:fq-name . "io.spring.api.ArticlesApi.getArticles-INT-INT-java.lang.String-java.lang.String-java.lang.String-User")
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
  (let ((ast-analyzer (make-ast-analyzer :java *java-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "p1/NewClassReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *java-path* "p1/NewClassReference.java"
                      '((:line . 7) (:offset . 9))))))
          (find-references ast-analyzer
                           `((:path . "p1/NewClassHelper.java")
                             (:name . "method")
                             (:fq-name . "p1.NewClassHelper.method")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-for-constructor
  (let ((ast-analyzer (make-ast-analyzer :java *java-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "p1/ConstructorReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *java-path* "p1/ConstructorReference.java"
                      '((:line . 7) (:offset . 9))))))
          (find-references ast-analyzer
                           `((:path . "p1/ConstructorHelper.java")
                             (:name . "ConstructorHelper")
                             (:fq-name . "p1.ConstructorHelper.ConstructorHelper-INT")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-for-private-method
  (let ((ast-analyzer (make-ast-analyzer :java *java-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "p1/PrivateMethodReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *java-path* "p1/PrivateMethodReference.java"
                      '((:line . 5) (:offset . 9))))))
          (find-references ast-analyzer
                           `((:path . "p1/PrivateMethodReference.java")
                             (:name . "method2")
                             (:fq-name . "p1.PrivateMethodReference.method2")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-for-rest-client
  (let ((ast-analyzer (make-ast-analyzer :java *java-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "p1/client/ClientRestTemplate.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *java-path* "p1/client/ClientRestTemplate.java"
                      '((:line . 15) (:offset . 16)))))
            ((:path . "p1/client/ClientRestTemplate.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *java-path* "p1/client/ClientRestTemplate.java"
                      '((:line . 23) (:offset . 16))))))
          (find-references ast-analyzer
                           `((:type . :rest-server)
                             (:path . "/path")
                             (:name . "GET")))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-for-kotlin-class
  (let ((ast-analyzer (make-ast-analyzer :java *java-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "p1/KotlinReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *java-path* "p1/KotlinReference.java"
                      '((:line . 9) (:offset . 9))))))
          (find-references ast-analyzer
                           '((:path . "p1/JavaReference.kt")
                             (:name . "method")
                             (:fq-name . "p1.JavaReference.method")))))
    (stop-ast-analyzer ast-analyzer)))

