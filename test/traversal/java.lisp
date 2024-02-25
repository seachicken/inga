(defpackage #:inga/test/traversal/java
  (:use #:cl
        #:fiveam
        #:inga/traversal
        #:inga/ast-index
        #:inga/test/helper))
(in-package #:inga/test/traversal/java)

(def-suite java)
(in-suite java)

(defparameter *java-path* (merge-pathnames "test/fixtures/java/"))
(defparameter *spring-boot-path* (merge-pathnames "test/fixtures/spring-boot-realworld-example-app/"))
(defparameter *lightrun-path* (merge-pathnames "test/fixtures/spring-tutorials/lightrun/"))
(defparameter *guava-modules* (merge-pathnames "test/fixtures/spring-tutorials/guava-modules/"))

(test find-definitions-with-inner-class-and-primitive
  (with-fixture jvm-ctx (*spring-boot-path* 'ast-index-disk)
    (is (equal
          `(((:type . :module-public)
             (:path . "src/main/java/io/spring/application/CursorPager.java")
             (:name . "CursorPager")
             (:fq-name . "io.spring.application.CursorPager.CursorPager-java.util.List-io.spring.application.CursorPager$Direction-BOOLEAN")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/io/spring/application/CursorPager.java" *spring-boot-path*)
                      '((:line . 12) (:offset . 10))))))
          (find-definitions
            (create-range "src/main/java/io/spring/application/CursorPager.java" :line 12))))))

(test find-definitions-for-constructor
  (with-fixture jvm-ctx (*java-path* 'ast-index-disk)
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
  (with-fixture jvm-ctx (*java-path* 'ast-index-disk)
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
  (with-fixture jvm-ctx (*java-path* 'ast-index-disk)
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
  (with-fixture jvm-ctx (*java-path* 'ast-index-disk)
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
  (with-fixture jvm-ctx (*java-path* 'ast-index-disk)
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

(test find-references-for-new-class
  (with-fixture jvm-ctx (*java-path* 'ast-index-disk)
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
  (with-fixture jvm-ctx (*java-path* 'ast-index-disk)
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
  (with-fixture jvm-ctx (*java-path* 'ast-index-disk)
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
  (with-fixture jvm-ctx (*spring-boot-path* 'ast-index-disk :include '("src/main/**"))
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
                      '((:line . 76) (:offset . 14)))))
            ((:path . "src/main/java/io/spring/application/ArticleQueryService.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames
                        "src/main/java/io/spring/application/ArticleQueryService.java"
                        *spring-boot-path*)
                      '((:line . 84) (:offset . 14)))))
            ((:path . "src/main/java/io/spring/application/ArticleQueryService.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames
                        "src/main/java/io/spring/application/ArticleQueryService.java"
                        *spring-boot-path*)
                      '((:line . 96) (:offset . 14)))))
            ((:path . "src/main/java/io/spring/application/CommentQueryService.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames
                        "src/main/java/io/spring/application/CommentQueryService.java"
                        *spring-boot-path*)
                      '((:line . 83) (:offset . 12))))) 
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
              (:fq-name . "io.spring.application.CursorPager.CursorPager-java.util.List-io.spring.application.CursorPager$Direction-BOOLEAN"))
            *index*)))))

(test find-fq-name-for-reference-with-enum
  (with-fixture jvm-ctx (*java-path* 'ast-index-memory)
    (let ((path "p1/EnumReference.java"))
      (is (equal
            "p1.EnumHelper.EnumHelper-p1.EnumHelper.Enum"
            ;; ↓
            ;; new EnumHelper(Enum.A);
            (inga/traversal/java::find-fq-name-for-reference
              (first (trav:get-asts (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 9)))
                                    '("NEW_CLASS")))
              path
              *index*))))))

(test find-fq-name-for-reference-with-new-class
  (with-fixture jvm-ctx (*spring-boot-path* 'ast-index-memory :include '("src/main/**"))
    (let ((path "src/main/java/io/spring/application/ArticleQueryService.java"))
      (is (equal
            "io.spring.application.CursorPager.CursorPager-java.util.ArrayList-io.spring.application.CursorPager$Direction-BOOLEAN"
            ;;        ↓
            ;; return new CursorPager<>(new ArrayList<>(), page.getDirection(), false);
            (find-fq-name
              (find-ast-in-ctx `((:path . ,path) (:line . 63) (:offset . 14)))
              path))))))

(test find-fq-name-for-reference-with-lib-args
  (with-fixture jvm-ctx (*lightrun-path* 'ast-index-memory)
    (let ((path "api-service/src/main/java/com/baeldung/apiservice/adapters/users/UserRepository.java"))
      (is (equal
            "org.springframework.web.client.RestTemplate.getForObject-java.net.URI-java.lang.Class"
            (find-fq-name
              ;;                                 ↓
              ;; return restTemplate.getForObject(uri, User.class);
              (find-ast-in-ctx `((:path . ,path) (:line . 25) (:offset . 45)))
              path))))))

(test find-fq-name-for-reference-with-member_select-member_select
  (with-fixture jvm-ctx (*lightrun-path* 'ast-index-memory)
    (let ((path "api-service/src/main/java/com/baeldung/apiservice/RequestIdGenerator.java"))
      (is (equal
            "java.lang.Class.getCanonicalName"
            ;;                                                  ↓
            ;; MDC.put(RequestIdGenerator.class.getCanonicalName(), requestId);
            (find-fq-name
              (find-ast-in-ctx `((:path . ,path) (:line . 19) (:offset . 58)))
              path))))))

(test find-fq-name-for-reference-with-array-args
  (with-fixture jvm-ctx (*guava-modules* 'ast-index-memory)
    (let ((path "guava-collections/src/test/java/com/baeldung/guava/ordering/GuavaOrderingExamplesUnitTest.java"))
      (is (equal
            "com.google.common.collect.Ordering.explicit-java.util.ArrayList"
            ;;                  ↓
            ;; Ordering.explicit(Lists.newArrayList("b", "zz", "aa", "ccc"));
            (find-fq-name
              (find-ast-in-ctx `((:path . ,path) (:line . 104) (:offset . 65)))
              path))))))

(test find-fq-name-for-factory-method
  (with-fixture jvm-ctx (*java-path* 'ast-index-memory)
    (let ((path "p1/TypeInferenceReference.java"))
      (is (equal
            "java.util.List.of-java.lang.String"
            ;;        ↓
            ;; List.of("a").forEach(v -> System.out.println(v));
            (find-fq-name (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 16))) path))))))

(test find-fq-name-for-array
  (with-fixture jvm-ctx (*java-path* 'ast-index-memory)
    (let ((path "p1/ArrayReference.java"))
      (is (equal
            "java.util.Arrays.copyOfRange-java.lang.String[]-INT-INT"
            ;;                   ↓
            ;; Arrays.copyOfRange(array, 1, array.length);
            (find-fq-name (find-ast-in-ctx `((:path . ,path) (:line . 8) (:offset . 27))) path))))))

(test find-fq-class-name-for-new-class
  (with-fixture jvm-ctx (*spring-boot-path* 'ast-index-memory :include '("src/main/**"))
    (let ((path "src/main/java/io/spring/application/ArticleQueryService.java"))
      (is (equal
            "io.spring.application.CursorPager"
            ;;        ↓
            ;; return new CursorPager<>(new ArrayList<>(), page.getDirection(), false);
            (find-fq-class-name
              (find-ast-in-ctx `((:path . ,path) (:line . 63) (:offset . 14)))
              path))))))

(test find-fq-class-name-for-primitive-parameter
  (with-fixture jvm-ctx (*spring-boot-path* 'ast-index-memory :include '("src/main/**"))
    (let ((path "src/main/java/io/spring/application/CursorPager.java"))
      (is (equal
            "BOOLEAN"
            ;;                                                               ↓
            ;; public CursorPager(List<T> data, Direction direction, boolean hasExtra) {
            (find-fq-class-name
              (find-ast-in-ctx `((:path . ,path) (:line . 12) (:offset . 65)))
              path))))))

(test find-fq-class-name-for-inner-class-parameter
  (with-fixture jvm-ctx (*spring-boot-path* 'ast-index-memory :include '("src/main/**"))
    (let ((path "src/main/java/io/spring/application/CursorPager.java"))
      (is (equal
            "io.spring.application.CursorPager$Direction"
            ;;                                            ↓
            ;; public CursorPager(List<T> data, Direction direction, boolean hasExtra) {
            (find-fq-class-name
              (find-ast-in-ctx `((:path . ,path) (:line . 12) (:offset . 46)))
              path))))))

(test find-fq-class-name-for-type-inference-in-lambda
  (with-fixture jvm-ctx (*java-path* 'ast-index-memory)
    (let ((path "p1/TypeInferenceReference.java"))
      (is (equal
            "java.lang.String"
            ;;                                              ↓
            ;; List.of("a").forEach(v -> System.out.println(v));
            (find-fq-class-name
              (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 54)))
              path))))))

(test find-fq-class-name-for-reference-type-inference-in-lambda
  (with-fixture jvm-ctx (*java-path* 'ast-index-memory)
    (let ((path "p1/TypeInferenceReference.java"))
      (is (equal
            "java.lang.String"
            (find-fq-class-name
              (find-ast-in-ctx `((:path . ,path) (:line . 12) (:offset . 46)))
              path))))))

(test find-references-for-kotlin-class
  (with-fixture jvm-ctx (*java-path* 'ast-index-disk)
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
  (with-fixture jvm-ctx (*lightrun-path* 'ast-index-disk)
    (is (equal
          '("api-service/src/main/java/com/baeldung/apiservice/adapters/http/TasksController.java")
          (inga/traversal::get-scoped-index-paths
            '((:type . :module-private)
              (:path . "api-service/src/main/java/com/baeldung/apiservice/adapters/http/TasksController.java")
              (:fq-name . "com.baeldung.apiservice.adapters.http.TasksController.getUser-java.lang.String")
              )
            *index*)))))

(test get-scoped-index-paths-with-module-public
  (with-fixture jvm-ctx (*lightrun-path* 'ast-index-disk)
    (is (null
          (find-if-not
            (lambda (p) (uiop:string-prefix-p "api-service" p))
            (inga/traversal::get-scoped-index-paths
              '((:type . :module-public)
                (:path . "api-service/src/main/java/com/baeldung/apiservice/adapters/users/UserRepository.java")
                (:fq-name . "com.baeldung.apiservice.adapters.users.UserRepository.getUserById-java.lang.String"))
              *index*))))))

(test matches-signature
  (with-fixture jvm-ctx (*java-path* 'ast-index-disk)
    (is (eq
          t
          (matches-signature
            "p2.ApiSignature-p2.ChildClass"
            "p2.ApiSignature-p2.ParentClass"
            *index*)))))

(test not-matches-signature-with-no-args
  (with-fixture jvm-ctx (*lightrun-path* 'ast-index-disk)
    (is (eq
          nil
          ;; https://spring.pleiades.io/spring-framework/docs/current/javadoc-api/org/springframework/web/util/UriComponentsBuilder.html
          (matches-signature
            "build"
            "build-boolean"
            *index*)))))

(test matches-signature-with-sub-class
  (with-fixture jvm-ctx (*spring-boot-path* 'ast-index-disk :include '("src/main/**"))
    (is (eq
          t
          (matches-signature
            "io.spring.application.CursorPager.CursorPager-java.util.ArrayList-io.spring.application.CursorPager$Direction-BOOLEAN"
            "io.spring.application.CursorPager.CursorPager-java.util.List-io.spring.application.CursorPager$Direction-BOOLEAN"
            *index*)))))

(test matches-signature-with-object-array
  (with-fixture jvm-ctx (*guava-modules* 'ast-index-memory)
    (is (eq
          t
          (matches-signature
            "com.google.common.collect.Lists.newArrayList-java.lang.String-java.lang.String"
            "com.google.common.collect.Lists.newArrayList-java.lang.Object[]"
            *index*)))))

(test not-matches-signature-with-object-array
  (with-fixture jvm-ctx (*lightrun-path* 'ast-index-disk)
    (is (eq
          nil
          (matches-signature
            "build"
            "build-java.lang.Object[]"
            *index*)))))

(test matches-signature-with-null
  (with-fixture jvm-ctx (*java-path* 'ast-index-memory)
    (is (eq
          t
          (matches-signature
            "java.lang.Object-equals-NULL"
            "java.lang.Object-equals-java.lang.Object"
            *index*)))))

(test matches-signature-with-wild-card
  (with-fixture jvm-ctx (*java-path* 'ast-index-memory)
    (is (eq
          t
          (matches-signature
            "java.lang.Object.wait-LONG-INT"
            "java.lang.Object.*"
            *index*)))))

(test find-class-hierarchy-with-standard-class
  (with-fixture jvm-ctx (*lightrun-path* 'ast-index-disk)
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
  (with-fixture jvm-ctx (*java-path* 'ast-index-disk)
    (is (equal
          '("java.lang.Object"
            "p2.ParentClass"
            "p2.ChildClass")
          (find-class-hierarchy "p2.ChildClass" *index*)))))

