(defpackage #:inga/test/parser/java
  (:use #:cl
        #:fiveam
        #:inga/parser))
(in-package #:inga/test/parser/java)

(def-suite java)
(in-suite java)

(defparameter *spring-boot-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-boot-realworld-example-app/")))

(test find-affected-pos-for-field-annotation
  (let ((parser (make-parser :java *spring-boot-path*)))
    (start-parser parser)
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

;; public class ArticleQueryService {
;;                                ↓[out]
;;   public Optional<ArticleData> findById(String id, User user) {
;;     ArticleData articleData = articleReadService.findById(id); ←[in]
;;   }
;; }
(test find-affected-pos-for-method
  (let ((parser (make-parser :java *spring-boot-path*)))
    (start-parser parser)
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
  (let ((parser (make-parser :java *spring-boot-path*)))
    (start-parser parser)
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

;; public class Article {
;;   public void update(String title, String description, String body) {
;;   } ←[in]
;; }
(test ignore-affected-pos-when-end-block
  (let ((parser (make-parser :java *spring-boot-path*)))
    (start-parser parser)
    (is (equal
          nil
          (let ((src-path "src/main/java/io/spring/core/article/Article.java"))
            (inga/parser/java::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              65))))
    (stop-parser parser)))

(test count-combinations
  (let ((parser (make-parser :java *spring-boot-path*)))
    (start-parser parser)
    (is (equal
          2
          (let ((src-path "src/main/java/io/spring/core/article/Article.java"))
            (inga/parser/typescript::count-combinations
              parser
              src-path
              (exec-parser parser src-path)
              '(53 54)))))
    (stop-parser parser)))

