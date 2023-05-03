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
(test find-affected-pos-for-method
  (let ((parser (make-parser :java *spring-boot-path* *cache*)))
    (start-parser parser nil nil)
    (is (equal
          '((:path . "src/main/java/io/spring/application/ArticleQueryService.java")
            (:name . "findById") (:line . 30) (:offset . 32))
          (let ((src-path "src/main/java/io/spring/application/ArticleQueryService.java"))
            (find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              31))))
    (stop-parser parser)))

;; public interface ArticleRepository {
;;        ↓[out]
;;   void save(Article article); ←[in]
;; }
(test find-affected-pos-for-interface
  (let ((parser (make-parser :java *spring-boot-path* *cache*)))
    (start-parser parser nil nil)
    (is (equal
          '((:path . "src/main/java/io/spring/core/article/ArticleRepository.java")
            (:name . "save") (:line . 7) (:offset . 8))
          (let ((src-path "src/main/java/io/spring/core/article/ArticleRepository.java"))
            (find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              7))))
    (stop-parser parser)))

;; public class NewArticleParam {
;;   @DuplicatedArticleConstraint ←[in]
;;                  ↓[out]
;;   private String title;
;; }
(test find-affected-pos-for-field-annotation
  (let ((parser (make-parser :java *spring-boot-path* *cache*)))
    (start-parser parser nil nil)
    (is (equal
          '((:path . "src/main/java/io/spring/application/article/NewArticleParam.java")
            (:name . "title") (:line . 19) (:offset . 18))
          (let ((src-path "src/main/java/io/spring/application/article/NewArticleParam.java"))
            (find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              18))))
    (stop-parser parser)))

;; class DuplicatedArticleValidator
;;                                    ↓[out]
;;     implements ConstraintValidator<DuplicatedArticleConstraint, String> {
;;   @Override
;;   public boolean isValid(String value, ConstraintValidatorContext context) {
;;     return true; ←[in]
;;   }
;; }
(test find-affected-pos-for-constraint-validator
  (let ((parser (make-parser :java *spring-boot-path* *cache*)))
    (start-parser parser nil nil)
    (is (equal
          '((:path . "src/main/java/io/spring/application/article/DuplicatedArticleValidator.java")
            (:name . "DuplicatedArticleConstraint") (:line . 10) (:offset . 36))
          (let ((src-path "src/main/java/io/spring/application/article/DuplicatedArticleValidator.java"))
            (find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              16))))
    (stop-parser parser)))

;; public class Article {
;;   public void update(String title, String description, String body) {
;;   } ←[in]
;; }
(test ignore-affected-pos-when-end-block
  (let ((parser (make-parser :java *spring-boot-path* *cache*)))
    (start-parser parser nil nil)
    (is (equal
          nil
          (let ((src-path "src/main/java/io/spring/core/article/Article.java"))
            (inga/parser/java::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              65))))
    (stop-parser parser)))

(test get-fq-name-of-declaration
  (let ((parser (make-parser :java *jvm-path* *cache*)))
    (start-parser parser nil nil)
    (is (equal
          "jvm.java.Class.method"
          (inga/parser/java::get-fq-name-of-declaration
            (exec-parser parser "java/Class.java")
            '((:path . "java/Class.java")
              (:name . "method") (:line . 6) (:offset . 17)))))
    (stop-parser parser)))

