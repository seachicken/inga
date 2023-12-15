(defpackage #:inga/test/traversal/java
  (:use #:cl
        #:fiveam
        #:inga/traversal
        #:inga/ast-index
        #:inga/test/helper))
(in-package #:inga/test/traversal/java)

(def-suite java)
(in-suite java)

(defparameter *java-path* (merge-pathnames #p"test/fixtures/java/"))
(defparameter *spring-boot-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-boot-realworld-example-app/")))
(defparameter *lightrun-path* (merge-pathnames "test/fixtures/spring-tutorials/lightrun/"))
(defparameter *guava-modules* (merge-pathnames "test/fixtures/spring-tutorials/guava-modules/"))

(test find-definitions-with-inner-class-and-primitive
  (with-fixture jvm-context (*spring-boot-path* 'ast-index-disk)
    (is (equal
          `(((:type . :module-public)
             (:path . "src/main/java/io/spring/application/CursorPager.java")
             (:name . "CursorPager")
             (:fq-name . "io.spring.application.CursorPager.CursorPager-java.util.List-io.spring.application.CursorPager.Direction-BOOLEAN")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/io/spring/application/CursorPager.java" *spring-boot-path*)
                      '((:line . 12) (:offset . 10))))))
          (find-definitions
            (create-range "src/main/java/io/spring/application/CursorPager.java" :line 12))))))

(test find-definitions-for-constructor
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
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
            (create-range "p1/ConstructorDefinition.java" :line 4))))))

(test find-definitions-for-method
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
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
            (create-range "p1/MethodDefinition.java" :line 7))))))

(test find-definitions-for-interface
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
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
            (create-range "p1/InterfaceDefinition.java" :line 6))))))

(test find-definitions-for-instance-variable-annotation
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
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
            (create-range "p1/InstanceVariableAnnotationDefinition.java" :line 6))))))

(test find-definitions-for-generic-type
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
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
            (create-range "p1/GenericTypeDefinition.java" :line 6))))))

(test find-definitions-for-spring-rest-controller
  (with-fixture jvm-context (*spring-boot-path* 'ast-index-disk)
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
            (create-range "src/main/java/io/spring/api/ArticlesApi.java" :line 56))))))

(test find-definitions-for-spring-rest-get-method
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
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
                        '((:line . 20) (:offset . 17))))))))
          (find-definitions
            (create-range "p1/server/spring/src/main/p1/RestControllerDefinition.java" :line 20))))))

(test find-definitions-for-spring-rest-get-method-with-request-mapping
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
    (is (equal
          `(((:type . :rest-server)
             (:host . "8080")
             (:name . "GET")
             (:path . "/request/{string}")
             (:file-pos .
              ((:type . :module-public)
               (:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
               (:name . "getWithRequest")
               (:fq-name . "p1.RestControllerDefinition.getWithRequest-java.lang.String")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                        '((:line . 16) (:offset . 17))))))))
          (find-definitions
            (create-range "p1/server/spring/src/main/p1/RestControllerDefinition.java" :line 16))))))

(test find-definitions-for-spring-rest-get-method-with-values
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
    (is (equal
          `(((:type . :rest-server)
             (:host . "8080")
             (:name . "GET")
             (:path . "/{string}/{string}")
             (:file-pos .
              ((:type . :module-public)
               (:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
               (:name . "getWithValues")
               (:fq-name . "p1.RestControllerDefinition.getWithValues-java.lang.String-java.lang.String")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                        '((:line . 44) (:offset . 17)))))))
            ((:type . :rest-server)
             (:host . "8080")
             (:name . "GET")
             (:path . "/{string}")
             (:file-pos .
              ((:type . :module-public)
               (:path . "p1/server/spring/src/main/p1/RestControllerDefinition.java")
               (:name . "getWithValues")
               (:fq-name . "p1.RestControllerDefinition.getWithValues-java.lang.String-java.lang.String")
               ,(cons :top-offset
                      (convert-to-top-offset
                        (merge-pathnames
                          "p1/server/spring/src/main/p1/RestControllerDefinition.java" *java-path*)
                        '((:line . 44) (:offset . 17))))))))
          (find-definitions
            (create-range "p1/server/spring/src/main/p1/RestControllerDefinition.java" :line 44))))))

(test find-definitions-for-spring-rest-post-method
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
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
                        '((:line . 24) (:offset . 17))))))))
          (find-definitions
            (create-range "p1/server/spring/src/main/p1/RestControllerDefinition.java" :line 24))))))

(test find-definitions-for-spring-rest-put-method
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
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
                        '((:line . 28) (:offset . 17))))))))
          (find-definitions
            (create-range "p1/server/spring/src/main/p1/RestControllerDefinition.java" :line 28))))))

(test find-definitions-for-spring-rest-delete-method
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
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
                        '((:line . 32) (:offset . 17))))))))
          (find-definitions
            (create-range "p1/server/spring/src/main/p1/RestControllerDefinition.java" :line 32))))))

(test find-references-for-new-class
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/NewClassReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/NewClassReference.java" *java-path*)
                      '((:line . 7) (:offset . 9))))))
          (find-references
            `((:path . "p1/NewClassHelper.java")
              (:name . "method")
              (:fq-name . "p1.NewClassHelper.method"))
            *index*)))))

(test find-references-for-constructor
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/ConstructorReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/ConstructorReference.java" *java-path*)
                      '((:line . 7) (:offset . 9))))))
          (find-references
            `((:path . "p1/ConstructorHelper.java")
              (:name . "ConstructorHelper")
              (:fq-name . "p1.ConstructorHelper.ConstructorHelper-INT"))
            *index*)))))

(test find-references-for-private-method
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/PrivateMethodReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/PrivateMethodReference.java" *java-path*)
                      '((:line . 5) (:offset . 9))))))
          (find-references
            `((:path . "p1/PrivateMethodReference.java")
              (:name . "method2")
              (:fq-name . "p1.PrivateMethodReference.method2"))
            *index*)))))

(test find-references-with-sub-class-args
  (with-fixture jvm-context (*spring-boot-path* 'ast-index-disk :include '("src/main/**"))
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
              (:fq-name . "io.spring.application.CursorPager.CursorPager-java.util.List-io.spring.application.CursorPager.Direction-BOOLEAN"))
            *index*)))))

(test find-fq-name-for-reference-with-enum
  (with-fixture jvm-context (*java-path* 'ast-index-memory)
    (let ((path "p1/client/ClientRestTemplate.java"))
      (is (equal
            "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-NULL-java.lang.Class"
            (inga/traversal/java::find-fq-name-for-reference
              ;;                             ↓
              ;; return restTemplate.exchange("http://localhost:8080/path", HttpMethod.GET, null, String.class);
              (find-ast path '((:line . 24) (:offset . 37)))
              path
              *index*))))))

(test find-fq-name-for-reference-with-new-class
  (with-fixture jvm-context (*spring-boot-path* 'ast-index-memory :include '("src/main/**"))
    (let ((path "src/main/java/io/spring/application/ArticleQueryService.java"))
      (is (equal
            "io.spring.application.CursorPager.CursorPager-java.util.ArrayList-io.spring.application.CursorPager.Direction-BOOLEAN"
            ;;        ↓
            ;; return new CursorPager<>(new ArrayList<>(), page.getDirection(), false);
            (inga/traversal/java::find-fq-name-for-reference
              (find-ast path '((:line . 63) (:offset . 14)))
              path
              *index*))))))

(test find-fq-name-for-reference-with-lib-args
  (with-fixture jvm-context (*lightrun-path* 'ast-index-memory)
    (let ((path "api-service/src/main/java/com/baeldung/apiservice/adapters/users/UserRepository.java"))
      (is (equal
            "org.springframework.web.client.RestTemplate.getForObject-java.net.URI-java.lang.Class"
            (inga/traversal/java::find-fq-name-for-reference
              ;;                                 ↓
              ;; return restTemplate.getForObject(uri, User.class);
              (find-ast path '((:line . 25) (:offset . 45)))
              path
              *index*))))))

(test find-fq-name-for-reference-with-member_select-member_select
  (with-fixture jvm-context (*lightrun-path* 'ast-index-memory)
    (let ((path "api-service/src/main/java/com/baeldung/apiservice/RequestIdGenerator.java"))
      (is (equal
            "java.lang.Class.getCanonicalName"
            ;;                                                  ↓
            ;; MDC.put(RequestIdGenerator.class.getCanonicalName(), requestId);
            (inga/traversal/java::find-fq-name-for-reference
              (find-ast path '((:line . 19) (:offset . 58)))
              path
              *index*))))))

;;(test find-fq-name-for-reference-with-array-args
;;  (with-fixture jvm-context (*guava-modules* 'ast-index-memory)
;;    (let ((path "guava-collections/src/test/java/com/baeldung/guava/ordering/GuavaOrderingExamplesUnitTest.java"))
;;      (is (equal
;;            "com.google.common.collect.Ordering.explicit-java.util.ArrayList"
;;            ;;                  ↓
;;            ;; Ordering.explicit(Lists.newArrayList("b", "zz", "aa", "ccc"));
;;            (inga/traversal/java::find-fq-name-for-reference
;;              (find-ast path '((:line . 104) (:offset . 65)))
;;              path
;;              *index*))))))

(test find-fq-class-name-for-same-package
  (with-fixture jvm-context (*spring-boot-path* 'ast-index-memory :include '("src/main/**"))
    (let ((path "src/main/java/io/spring/application/ArticleQueryService.java"))
      (is (equal
            "io.spring.application.CursorPager"
            (inga/traversal/java::find-fq-class-name-by-class-name
              "CursorPager"
              (find-ast path '((:line . 63) (:offset . 14)))))))))

(test find-fq-class-name-for-inner-class
  (with-fixture jvm-context (*spring-boot-path* 'ast-index-memory :include '("src/main/**"))
    (let ((path "src/main/java/io/spring/application/CursorPager.java"))
      (is (equal
            "io.spring.application.CursorPager.Direction"
            (inga/traversal/java::find-fq-class-name-by-class-name
              "Direction"
              (find-ast path '((:line . 12) (:offset . 46)))))))))

(test find-references-for-rest-client-get-method
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
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
              (:name . "GET"))
            *index*)))))

(test find-references-for-rest-client-post-method
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
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
              (:name . "POST"))
            *index*)))))

(test find-references-for-kotlin-class
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/KotlinReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/KotlinReference.java" *java-path*)
                      '((:line . 9) (:offset . 9))))))
          (find-references
            '((:path . "p1/JavaReference.kt")
              (:name . "method")
              (:fq-name . "p1.JavaReference.method"))
            *index*)))))

(test get-scoped-index-paths-with-module-private
  (with-fixture jvm-context (*lightrun-path* 'ast-index-disk)
    (is (equal
          '("api-service/src/main/java/com/baeldung/apiservice/adapters/http/TasksController.java")
          (inga/traversal::get-scoped-index-paths
            '((:type . :module-private)
              (:path . "api-service/src/main/java/com/baeldung/apiservice/adapters/http/TasksController.java")
              (:fq-name . "com.baeldung.apiservice.adapters.http.TasksController.getUser-java.lang.String")
              )
            *index*)))))

(test get-scoped-index-paths-with-module-public
  (with-fixture jvm-context (*lightrun-path* 'ast-index-disk)
    (is (null
          (find-if-not
            (lambda (p) (uiop:string-prefix-p "api-service" p))
            (inga/traversal::get-scoped-index-paths
              '((:type . :module-public)
                (:path . "api-service/src/main/java/com/baeldung/apiservice/adapters/users/UserRepository.java")
                (:fq-name . "com.baeldung.apiservice.adapters.users.UserRepository.getUserById-java.lang.String"))
              *index*))))))

(test matches-signature
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
    (is (eq
          t
          (matches-signature
            "p2.ApiSignature-p2.ChildClass"
            "p2.ApiSignature-p2.ParentClass"
            *index*)))))

(test not-matches-signature-with-no-args
  (with-fixture jvm-context (*lightrun-path* 'ast-index-disk)
    (is (eq
          nil
          ;; https://spring.pleiades.io/spring-framework/docs/current/javadoc-api/org/springframework/web/util/UriComponentsBuilder.html
          (matches-signature
            "build"
            "build-boolean"
            *index*)))))

(test matches-signature-with-sub-class
  (with-fixture jvm-context (*spring-boot-path* 'ast-index-disk :include '("src/main/**"))
    (is (eq
          t
          (matches-signature
            "io.spring.application.CursorPager.CursorPager-java.util.ArrayList-io.spring.application.CursorPager.Direction-BOOLEAN"
            "io.spring.application.CursorPager.CursorPager-java.util.List-io.spring.application.CursorPager.Direction-BOOLEAN"
            *index*)))))

(test matches-signature-with-object-array
  (with-fixture jvm-context (*guava-modules* 'ast-index-memory)
    (is (eq
          t
          (matches-signature
            "com.google.common.collect.Lists.newArrayList-java.lang.String-java.lang.String"
            "com.google.common.collect.Lists.newArrayList-java.lang.Object[]"
            *index*)))))

(test not-matches-signature-with-object-array
  (with-fixture jvm-context (*lightrun-path* 'ast-index-disk)
    (is (eq
          nil
          (matches-signature
            "build"
            "build-java.lang.Object[]"
            *index*)))))

(test find-class-hierarchy-with-standard-class
  (with-fixture jvm-context (*lightrun-path* 'ast-index-disk)
    (is (equal
          '("java.io.Serializable"
            "java.lang.Comparable"
            "java.lang.CharSequence"
            "java.lang.constant.Constable"
            "java.lang.constant.ConstantDesc"
            "java.lang.Object"
            "java.lang.String")
          (find-class-hierarchy "java.lang.String" *index*)))))

(test find-class-hierarchy-with-app-class
  (with-fixture jvm-context (*java-path* 'ast-index-disk)
    (is (equal
          '("java.lang.Object"
            "p2.ParentClass"
            "p2.ChildClass")
          (find-class-hierarchy "p2.ChildClass" *index*)))))

