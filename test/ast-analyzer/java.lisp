(defpackage #:inga/test/ast-analyzer/java
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer))
(in-package #:inga/test/ast-analyzer/java)

(def-suite java)
(in-suite java)

(defparameter *jvm-path* (merge-pathnames #p"test/fixtures/jvm/"))
(defparameter *spring-boot-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-boot-realworld-example-app/")))
(defparameter *cache* (inga/cache:make-cache 100))

(test find-definitions-for-constructor
  (let ((ast-analyzer (make-ast-analyzer :java *jvm-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "java/ConstructorClass.java")
             (:name . "ConstructorClass")
             (:fq-name . "jvm.java.ConstructorClass.ConstructorClass")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path*
                      "java/ConstructorClass.java"
                      '((:line . 4) (:offset . 10))))))
          (find-definitions
            ast-analyzer
            `((:path . "java/ConstructorClass.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *jvm-path*
                       "java/ConstructorClass.java"
                       '((:line . 4) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *jvm-path*
                       "java/ConstructorClass.java"
                       '((:line . 4) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

;; public class ArticleQueryService {
;;                                ↓[out]
;;   public Optional<ArticleData> findById(String id, User user) {
;;     ArticleData articleData = articleReadService.findById(id); ←[in]
;;   }
;; }
(test find-definitions-for-method
  (let ((ast-analyzer (make-ast-analyzer :java *spring-boot-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "src/main/java/io/spring/application/ArticleQueryService.java")
             (:name . "findById")
             (:fq-name . "io.spring.application.ArticleQueryService.findById-String-User")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *spring-boot-path*
                      "src/main/java/io/spring/application/ArticleQueryService.java"
                      '((:line . 30) (:offset . 32))))))
          (find-definitions
            ast-analyzer
            `((:path . "src/main/java/io/spring/application/ArticleQueryService.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *spring-boot-path*
                       "src/main/java/io/spring/application/ArticleQueryService.java"
                       '((:line . 31) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *spring-boot-path*
                       "src/main/java/io/spring/application/ArticleQueryService.java"
                       '((:line . 31) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

;; public interface ArticleRepository {
;;        ↓[out]
;;   void save(Article article); ←[in]
;; }
(test find-definitions-for-interface
  (let ((ast-analyzer (make-ast-analyzer :java *spring-boot-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "src/main/java/io/spring/core/article/ArticleRepository.java")
             (:name . "save")
             (:fq-name . "io.spring.core.article.ArticleRepository.save-Article")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *spring-boot-path*
                      "src/main/java/io/spring/core/article/ArticleRepository.java"
                      '((:line . 7) (:offset . 8))))))
          (find-definitions
            ast-analyzer
            `((:path . "src/main/java/io/spring/core/article/ArticleRepository.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *spring-boot-path*
                       "src/main/java/io/spring/core/article/ArticleRepository.java"
                       '((:line . 7) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *spring-boot-path*
                       "src/main/java/io/spring/core/article/ArticleRepository.java"
                       '((:line . 7) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

;; public class NewArticleParam {
;;   @DuplicatedArticleConstraint ←[in]
;;                  ↓[out]
;;   private String title;
;; }
(test find-definitions-for-field-annotation
  (let ((ast-analyzer (make-ast-analyzer :java *spring-boot-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "src/main/java/io/spring/application/article/NewArticleParam.java")
             (:name . "title")
             (:fq-name . "io.spring.application.article.NewArticleParam.title")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *spring-boot-path*
                      "src/main/java/io/spring/application/article/NewArticleParam.java"
                      '((:line . 19) (:offset . 18))))))
          (find-definitions
            ast-analyzer
            `((:path . "src/main/java/io/spring/application/article/NewArticleParam.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *spring-boot-path*
                       "src/main/java/io/spring/application/article/NewArticleParam.java"
                       '((:line . 18) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *spring-boot-path*
                       "src/main/java/io/spring/application/article/NewArticleParam.java"
                       '((:line . 18) (:offset . -1))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-definitions-for-overload
  (let ((ast-analyzer (make-ast-analyzer :java *jvm-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          `(((:path . "java/Overload.java")
             (:name . "method")
             (:fq-name . "jvm.java.Overload.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path*
                      "java/Overload.java"
                      '((:line . 4) (:offset . 19))))))
          (find-definitions
            ast-analyzer
            `((:path . "java/Overload.java")
              ,(cons :start-offset
                     (convert-to-top-offset
                       *jvm-path*
                       "java/Overload.java"
                       '((:line . 5) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       *jvm-path*
                       "java/Overload.java"
                       '((:line . 5) (:offset . -1))))))))
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

(test find-references-to-imported-method
  (let ((ast-analyzer (make-ast-analyzer :java *jvm-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "java/Class.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path* "java/Class.java"
                      '((:line . 12) (:offset . 23))))))
          (find-references ast-analyzer
                           `((:path . "java/Overload.java")
                             (:name . "method")
                             (:fq-name . "jvm.java.Overload.method-INT")
                             (:top-offset ,(convert-to-top-offset
                                             *jvm-path*
                                             "java/Overload.java"
                                             '((:line . 4) (:offset . 19))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-to-constructor
  (let ((ast-analyzer (make-ast-analyzer :java *jvm-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "java/NewClass.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path* "java/NewClass.java"
                      '((:line . 7) (:offset . 9))))))
          (find-references ast-analyzer
                           `((:path . "java/ConstructorClass.java")
                             (:name . "ConstructorClass")
                             (:fq-name . "jvm.java.ConstructorClass.ConstructorClass-INT")
                             (:top-offset ,(convert-to-top-offset
                                             *jvm-path*
                                             "java/ConstructorClass.java"
                                             '((:line . 7) (:offset . 10))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-to-private-method
  (let ((ast-analyzer (make-ast-analyzer :java *jvm-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "java/Class.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path* "java/Class.java"
                      '((:line . 13) (:offset . 9))))))
          (find-references ast-analyzer
                           `((:path . "java/Class.java")
                             (:name . "method2")
                             (:fq-name . "jvm.java.Class.method2")
                             (:top-offset ,(convert-to-top-offset
                                             *jvm-path*
                                             "java/Class.java"
                                             '((:line . 16) (:offset . 17))))))))
    (stop-ast-analyzer ast-analyzer)))

(test the-return-name-is-not-included-in-references
  (let ((ast-analyzer (make-ast-analyzer :java *jvm-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          nil 
          (find-references ast-analyzer
                           `((:path . "java/Class.java")
                             (:name . "variable")
                             (:fq-name . "jvm.java.Class.variable")
                             (:top-offset ,(convert-to-top-offset
                                             *jvm-path*
                                             "java/Class.java"
                                             '((:line . 18) (:offset . 18))))))))
    (stop-ast-analyzer ast-analyzer)))

(test find-references-to-overload-method
  (let ((ast-analyzer (make-ast-analyzer :java *jvm-path* *cache*)))
    (start-ast-analyzer ast-analyzer inga/main::*include-java* nil)
    (is (equal
          `(((:path . "java/Overload.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      *jvm-path* "java/Overload.java"
                      '((:line . 5) (:offset . 16))))))
          (find-references ast-analyzer
                           `((:path . "java/Overload.java")
                             (:name . "method")
                             (:fq-name . "jvm.java.Overload.method-String")
                             (:top-offset ,(convert-to-top-offset
                                             *jvm-path*
                                             "java/Overload.java"
                                             '((:line . 8) (:offset . 19))))))))
    (stop-ast-analyzer ast-analyzer)))

(test get-fq-name-of-declaration
  (let ((ast-analyzer (make-ast-analyzer :java *jvm-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          "jvm.java.Class.method2"
          (inga/ast-analyzer/java::get-fq-name-of-declaration
            (cdr (jsown:parse (uiop:read-file-string (get-index-path "java/Class.java"))))
            (convert-to-top-offset
              *jvm-path* "java/Class.java"
              '((:line . 16) (:offset . 17))))))
    (stop-ast-analyzer ast-analyzer)))

(test get-fq-name-of-declaration-with-params
  (let ((ast-analyzer (make-ast-analyzer :java *jvm-path* *cache*)))
    (start-ast-analyzer ast-analyzer nil nil)
    (is (equal
          "jvm.java.Overload.method-INT"
          (inga/ast-analyzer/java::get-fq-name-of-declaration
            (cdr (jsown:parse (uiop:read-file-string (get-index-path "java/Overload.java"))))
            (convert-to-top-offset
              *jvm-path* "java/Overload.java"
              '((:line . 4) (:offset . 19))))))
    (stop-ast-analyzer ast-analyzer)))

