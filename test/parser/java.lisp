(defpackage #:inga/test/parser/java
  (:use #:cl
        #:fiveam
        #:inga/parser))
(in-package #:inga/test/parser/java)

(def-suite java)
(in-suite java)

(defparameter *jvm-path* (merge-pathnames #p"test/fixtures/jvm/"))
(defparameter *spring-boot-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-boot-realworld-example-app/")))
(defparameter *cache* (inga/cache:make-cache 100))

;; public class ArticleQueryService {
;;                                ↓[out]
;;   public Optional<ArticleData> findById(String id, User user) {
;;     ArticleData articleData = articleReadService.findById(id); ←[in]
;;   }
;; }
(test find-affected-poss-for-method
  (let ((parser (make-parser :java *spring-boot-path* *cache*)))
    (start-parser parser nil nil)
    (is (equal
          '(((:fq-name . "io.spring.application.ArticleQueryService.findById")
             (:name . "findById")
             (:path . "src/main/java/io/spring/application/ArticleQueryService.java")
             (:line . 30) (:offset . 32)))
          (find-affected-poss
            parser
            '((:path . "src/main/java/io/spring/application/ArticleQueryService.java")
              (:start . 31) (:end . 31)))))
    (stop-parser parser)))

;; public interface ArticleRepository {
;;        ↓[out]
;;   void save(Article article); ←[in]
;; }
(test find-affected-poss-for-interface
  (let ((parser (make-parser :java *spring-boot-path* *cache*)))
    (start-parser parser nil nil)
    (is (equal
          '(((:fq-name . "io.spring.core.article.ArticleRepository.save")
             (:name . "save")
             (:path . "src/main/java/io/spring/core/article/ArticleRepository.java")
             (:line . 7) (:offset . 8)))
          (find-affected-poss
            parser
            '((:path . "src/main/java/io/spring/core/article/ArticleRepository.java")
              (:start . 7) (:end . 7)))))
    (stop-parser parser)))

;; public class NewArticleParam {
;;   @DuplicatedArticleConstraint ←[in]
;;                  ↓[out]
;;   private String title;
;; }
(test find-affected-poss-for-field-annotation
  (let ((parser (make-parser :java *spring-boot-path* *cache*)))
    (start-parser parser nil nil)
    (is (equal
          '(((:fq-name . "io.spring.application.article.NewArticleParam.title")
             (:name . "title")
             (:path . "src/main/java/io/spring/application/article/NewArticleParam.java")
             (:line . 19) (:offset . 18)))
          (find-affected-poss
            parser
            '((:path . "src/main/java/io/spring/application/article/NewArticleParam.java")
              (:start . 18) (:end . 18)))))
    (stop-parser parser)))

;; class DuplicatedArticleValidator
;;                                    ↓[out]
;;     implements ConstraintValidator<DuplicatedArticleConstraint, String> {
;;   @Override
;;   public boolean isValid(String value, ConstraintValidatorContext context) {
;;     return true; ←[in]
;;   }
;; }
(test find-affected-poss-for-constraint-validator
  (if t
      (skip "TODO: implement")
      (let ((parser (make-parser :java *spring-boot-path* *cache*)))
        (start-parser parser nil nil)
        (is (equal
              '(((:path . "src/main/java/io/spring/application/article/DuplicatedArticleValidator.java")
                 (:name . "DuplicatedArticleConstraint") (:line . 10) (:offset . 36)))
              (find-affected-poss
                parser
                '((:path . "src/main/java/io/spring/application/article/DuplicatedArticleValidator.java")
                  (:start . 16) (:end . 16)))))
        (stop-parser parser))))

;; public class Article {
;;   public void update(String title, String description, String body) {
;;   } ←[in]
;; }
(test ignore-affected-poss-when-end-block
  (if t
      (skip "TODO: implement")
      (let ((parser (make-parser :java *spring-boot-path* *cache*)))
        (start-parser parser nil nil)
        (is (equal
              nil
              (find-affected-poss
                parser
                '((:path . "src/main/java/io/spring/core/article/Article.java")
                (:start . 65) (:end . 65)))))
        (stop-parser parser))))

(test get-fq-name-of-declaration
  (let ((parser (make-parser :java *jvm-path* *cache*)))
    (start-parser parser nil nil)
    (is (equal
          "jvm.java.Class.method2"
          (inga/parser/java::get-fq-name-of-declaration
            (exec-parser parser "java/Class.java")
            '((:path . "java/Class.java")
              (:name . "method2") (:line . 13) (:offset . 17))
            *jvm-path*)))
    (stop-parser parser)))

