(defpackage #:inga/test/analyzer/java
  (:use #:cl
        #:fiveam
        #:inga/analyzer
        #:inga/ast-index
        #:inga/test/helper))
(in-package #:inga/test/analyzer/java)

(def-suite java)

(def-suite jdk21
           :in java)
(in-suite jdk21)

(defparameter *java-path* (merge-pathnames "test/fixtures/general/"))

(test find-definitions-for-constructor
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          `(((:type . :module-public)
             (:path . "src/main/java/p1/ConstructorDefinition.java")
             (:name . "ConstructorDefinition")
             (:fq-name . "p1.ConstructorDefinition.ConstructorDefinition")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/p1/ConstructorDefinition.java" *java-path*)
                      '((:line . 4) (:offset . 12))))))
          (find-definitions
            (create-range "src/main/java/p1/ConstructorDefinition.java" :line 4))))))

(test find-definitions-for-method
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          `(((:type . :module-public)
             (:path . "src/main/java/p1/MethodDefinition.java")
             (:name . "method")
             (:fq-name . "p1.MethodDefinition.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/p1/MethodDefinition.java" *java-path*)
                      '((:line . 7) (:offset . 17))))))
          (find-definitions
            (create-range "src/main/java/p1/MethodDefinition.java" :line 7))))))

(test find-definitions-with-final-parameter
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          `(((:type . :module-public)
             (:path . "src/main/java/p1/MethodDefinition.java")
             (:name . "methodWithFinal")
             (:fq-name . "p1.MethodDefinition.methodWithFinal-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/p1/MethodDefinition.java" *java-path*)
                      '((:line . 10) (:offset . 17))))))
          (find-definitions
            (create-range "src/main/java/p1/MethodDefinition.java" :line 10))))))

(test find-definitions-for-interface
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          `(((:type . :module-default)
             (:path . "src/main/java/p1/InterfaceDefinition.java")
             (:name . "method")
             (:fq-name . "p1.InterfaceDefinition.method-INT")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/p1/InterfaceDefinition.java" *java-path*)
                      '((:line . 6) (:offset . 10))))))
          (find-definitions
            (create-range "src/main/java/p1/InterfaceDefinition.java" :line 6))))))

(test find-definitions-for-instance-variable-annotation
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          `(((:type . :module-public)
             (:path . "src/main/java/p1/InstanceVariableAnnotationDefinition.java")
             (:name . "variable")
             (:fq-name . "p1.InstanceVariableAnnotationDefinition.variable")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/p1/InstanceVariableAnnotationDefinition.java" *java-path*)
                      '((:line . 7) (:offset . 19))))))
          (find-definitions
            (create-range "src/main/java/p1/InstanceVariableAnnotationDefinition.java" :line 6))))))

(test find-definitions-for-generic-type
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          `(((:type . :module-public)
             (:path . "src/main/java/p1/GenericTypeDefinition.java")
             (:name . "GenericTypeDefinition")
             (:fq-name . "p1.GenericTypeDefinition.GenericTypeDefinition-java.util.List")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/p1/GenericTypeDefinition.java" *java-path*)
                      '((:line . 6) (:offset . 12))))))
          (find-definitions
            (create-range "src/main/java/p1/GenericTypeDefinition.java" :line 6))))))

(test find-definition-of-field-reference
  (with-fixture jvm-ctx (*java-path*)
    (let ((path "src/main/java/p1/FieldDefinition.java"))
      (is (equal
            ;;                ↓
            ;; private String field;
            (convert-to-top-offset
              (merge-pathnames path *java-path*) '((:line . 4) (:offset . 20)))
            ;;     ↓
            ;; this.field = p;
            (ast-value
              (find-definition
                "field"
                (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 13))))
              "pos"))))))

(test find-definition-of-parameter
  (with-fixture jvm-ctx (*java-path*)
    (let ((path "src/main/java/p1/FieldDefinition.java"))
      (is (equal
            ;;                           ↓
            ;; public void method(String p) {
            (convert-to-top-offset
              (merge-pathnames path *java-path*) '((:line . 6) (:offset . 31)))
            ;;              ↓
            ;; this.field = p;
            (ast-value
              (find-definition
                "p"
                (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 22))))
              "pos"))))))

(test find-references-for-new-class
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          `(((:path . "src/main/java/p1/NewClassReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/p1/NewClassReference.java" *java-path*)
                      '((:line . 7) (:offset . 9))))))
          (find-references
            `((:path . "src/main/java/p1/NewClassHelper.java")
              (:name . "method")
              (:fq-name . "p1.NewClassHelper.method"))
            *index*)))))

(test find-references-for-constructor
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          `(((:path . "src/main/java/p1/ConstructorReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/p1/ConstructorReference.java" *java-path*)
                      '((:line . 7) (:offset . 9))))))
          (find-references
            `((:path . "src/main/java/p1/ConstructorHelper.java")
              (:name . "ConstructorHelper")
              (:fq-name . "p1.ConstructorHelper.ConstructorHelper-INT"))
            *index*)))))

(test find-references-for-private-method
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          `(((:path . "src/main/java/p1/PrivateMethodReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/p1/PrivateMethodReference.java" *java-path*)
                      '((:line . 8) (:offset . 9))))))
          (find-references
            `((:path . "src/main/java/p1/PrivateMethodReference.java")
              (:name . "method2")
              (:fq-name . "p1.PrivateMethodReference.method2"))
            *index*)))))

(test find-references-with-stdlib-param
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          `(((:path . "src/main/java/p1/PrivateMethodReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/p1/PrivateMethodReference.java" *java-path*)
                      '((:line . 15) (:offset . 9))))))
          (find-references
            `((:path . "src/main/java/p1/PrivateMethodReference.java")
              (:name . "methodWithStdLib")
              (:fq-name . "p1.PrivateMethodReference.methodWithStdLib-java.nio.file.Path"))
            *index*)))))

(test find-reference-to-literal-field
  (with-fixture jvm-ctx (*java-path*)
    (let ((path "src/main/java/p1/LiteralReference.java"))
      (is (equal
            `(((:path . ,path)
               (:name . "field literal")
               (:top-offset .
                ,(convert-to-top-offset
                   (merge-pathnames path *java-path*) '((:line . 4) (:offset . 43))))))
            (find-reference-to-literal
              ;;        ↓
              ;; method(literal);
              (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 16)))
              path))))))

(test find-caller
  (with-fixture jvm-ctx (*java-path*)
    (let ((path "src/main/java/p1/ObjectCallReference.java"))
      (is (equal
            ;;                  ↓
            ;; client.methodSelf();
            ;; client.method();
            (convert-to-top-offset
              (merge-pathnames path *java-path*) '((:line . 6) (:offset . 26)))
            (ast-value
              (find-caller
                '(((:fq-name . "p1.ObjectCallHelper.methodSelf")))
                ;; client.methodSelf();
                ;;              ↓
                ;; client.method();
                (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 22)))
                path)
              "pos"))))))

(test find-caller-with-method-chains
  (with-fixture jvm-ctx (*java-path*)
    (let ((path "src/main/java/p1/ObjectCallReference.java"))
      (is (equal
            ;;                  ↓
            ;; client.methodSelf()
            ;;       .method();
            (convert-to-top-offset
              (merge-pathnames path *java-path*) '((:line . 12) (:offset . 26)))
            (ast-value
              (find-caller
                '(((:fq-name . "p1.ObjectCallHelper.methodSelf")))
                ;; client.methodSelf()
                ;;              ↓
                ;;       .method();
                (find-ast-in-ctx `((:path . ,path) (:line . 13) (:offset . 24)))
                path)
              "pos"))))))

(test find-fq-name-for-enum
  (with-fixture jvm-ctx (*java-path*)
    (let ((path "src/main/java/p1/EnumReference.java"))
      (is (equal
            "p1.EnumHelper.EnumHelper-p1.EnumHelper.Enum"
            ;; ↓
            ;; new EnumHelper(Enum.A);
            (find-fq-name
              (first (get-asts (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 9)))
                               '("NEW_CLASS")))
              path))))))

(test find-fq-name-for-factory-method
  (with-fixture jvm-ctx (*java-path*)
    (let ((path "src/main/java/p1/TypeInferenceReference.java"))
      (is (equal
            "java.util.List.of-java.lang.String"
            ;;        ↓
            ;; List.of("a").forEach(v -> System.out.println(v));
            (find-fq-name (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 16))) path))))))

(test find-fq-name-for-array
  (with-fixture jvm-ctx (*java-path*)
    (let ((path "src/main/java/p1/ArrayReference.java"))
      (is (equal
            "java.util.Arrays.copyOfRange-java.lang.String[]-INT-INT"
            ;;                   ↓
            ;; Arrays.copyOfRange(array, 1, array.length);
            (find-fq-name (find-ast-in-ctx `((:path . ,path) (:line . 8) (:offset . 27))) path))))))

(test find-fq-class-name-for-type-inference-in-lambda
  (with-fixture jvm-ctx (*java-path*)
    (let ((path "src/main/java/p1/TypeInferenceReference.java"))
      (is (equal
            "java.lang.String"
            ;;                                              ↓
            ;; List.of("a").forEach(v -> System.out.println(v));
            (find-fq-class-name
              (find-ast-in-ctx `((:path . ,path) (:line . 7) (:offset . 54)))
              path))))))

(test find-fq-class-name-for-reference-type-inference-in-lambda
  (with-fixture jvm-ctx (*java-path*)
    (let ((path "src/main/java/p1/TypeInferenceReference.java"))
      (is (equal
            "java.lang.String"
            (find-fq-class-name
              (find-ast-in-ctx `((:path . ,path) (:line . 12) (:offset . 46)))
              path))))))

(test find-references-for-kotlin-class
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          `(((:path . "src/main/java/p1/KotlinReference.java")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/main/java/p1/KotlinReference.java" *java-path*)
                      '((:line . 9) (:offset . 9))))))
          (find-references
            '((:path . "src/main/java/pkt1/JavaReference.kt")
              (:name . "method")
              (:fq-name . "pkt1.JavaReference.method"))
            *index*)))))

(test find-signature-without-array
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          '((:fq-name . "java.util.List.of-java.lang.Object"))
          (find-signature
            "java.util.List.of-java.lang.String"
            #'(lambda (fqcn) '(((:fq-name . "java.util.List.of-java.lang.Object"))
                               ((:fq-name . "java.util.List.of-java.lang.Object[]"))))
            "src/main/java/p1/NewClassReference.java")))))

(test matches-signature
  (with-fixture jvm-ctx (*java-path*)
    (is (eq
          t
          (inga/analyzer/base::matches-signature
            "p2.ApiSignature-p2.ChildClass"
            "p2.ApiSignature-p2.ParentClass"
            "src/main/java/p2/ApiSignature.java")))))

(test matches-signature-with-null
  (with-fixture jvm-ctx (*java-path*)
    (is (eq
          t
          (inga/analyzer/base::matches-signature
            "java.lang.Object-equals-NULL"
            "java.lang.Object-equals-java.lang.Object"
            "src/main/java/p2/ApiSignature.java")))))

(test matches-signature-with-additional-strings
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          t
          ;; https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/nio/file/Paths.html#get(java.lang.String,java.lang.String...)
          (inga/analyzer/base::matches-signature
            "method-java.lang.String"
            "method-java.lang.String-java.lang.String[]"
            "src/main/java/p2/ApiSignature.java")))))

(test find-class-hierarchy-with-app-class
  (with-fixture jvm-ctx (*java-path*)
    (is (equal
          '("java.lang.Object"
            "p2.ParentClass"
            "p2.ChildClass")
          (find-class-hierarchy "p2.ChildClass" "src/main/java/p2/ApiSignature.java")))))

(def-suite jdk17
           :in java)
(in-suite jdk17)

(defparameter *spring-boot-path* (merge-pathnames "test/fixtures/spring-boot-realworld-example-app/"))
(defparameter *lightrun-path* (merge-pathnames "test/fixtures/spring-tutorials/lightrun/"))
(defparameter *guava-modules* (merge-pathnames "test/fixtures/spring-tutorials/guava-modules/"))

(test find-definitions-with-inner-class-and-primitive
  (with-fixture jvm-ctx (*spring-boot-path*)
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

(test find-references-with-sub-class-args
  (with-fixture jvm-ctx (*spring-boot-path* :include '("src/main/**"))
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

(test find-fq-name-for-reference-with-new-class
  (with-fixture jvm-ctx (*spring-boot-path* :include '("src/main/**"))
    (let ((path "src/main/java/io/spring/application/ArticleQueryService.java"))
      (is (equal
            "io.spring.application.CursorPager.CursorPager-java.util.ArrayList-io.spring.application.CursorPager$Direction-BOOLEAN"
            ;;        ↓
            ;; return new CursorPager<>(new ArrayList<>(), page.getDirection(), false);
            (find-fq-name
              (find-ast-in-ctx `((:path . ,path) (:line . 63) (:offset . 14)))
              path))))))

(test find-fq-name-for-reference-with-lib-args
  (with-fixture jvm-ctx (*lightrun-path*)
    (let ((path "api-service/src/main/java/com/baeldung/apiservice/adapters/users/UserRepository.java"))
      (is (equal
            "org.springframework.web.client.RestTemplate.getForObject-java.net.URI-java.lang.Class"
            (find-fq-name
              ;;                                 ↓
              ;; return restTemplate.getForObject(uri, User.class);
              (find-ast-in-ctx `((:path . ,path) (:line . 25) (:offset . 45)))
              path))))))

(test find-fq-name-for-reference-with-member_select-member_select
  (with-fixture jvm-ctx (*lightrun-path*)
    (let ((path "api-service/src/main/java/com/baeldung/apiservice/RequestIdGenerator.java"))
      (is (equal
            "java.lang.Class.getCanonicalName"
            ;;                                                  ↓
            ;; MDC.put(RequestIdGenerator.class.getCanonicalName(), requestId);
            (find-fq-name
              (find-ast-in-ctx `((:path . ,path) (:line . 19) (:offset . 58)))
              path))))))

(test find-fq-name-for-reference-with-array-args
  (with-fixture jvm-ctx (*guava-modules*)
    (let ((path "guava-collections/src/test/java/com/baeldung/guava/ordering/GuavaOrderingExamplesUnitTest.java"))
      (is (equal
            "com.google.common.collect.Ordering.explicit-java.util.ArrayList"
            ;;                  ↓
            ;; Ordering.explicit(Lists.newArrayList("b", "zz", "aa", "ccc"));
            (find-fq-name
              (find-ast-in-ctx `((:path . ,path) (:line . 104) (:offset . 65)))
              path))))))

(test find-fq-class-name-for-new-class
  (with-fixture jvm-ctx (*spring-boot-path* :include '("src/main/**"))
    (let ((path "src/main/java/io/spring/application/ArticleQueryService.java"))
      (is (equal
            "io.spring.application.CursorPager"
            ;;        ↓
            ;; return new CursorPager<>(new ArrayList<>(), page.getDirection(), false);
            (find-fq-class-name
              (find-ast-in-ctx `((:path . ,path) (:line . 63) (:offset . 14)))
              path))))))

(test find-fq-class-name-for-primitive-parameter
  (with-fixture jvm-ctx (*spring-boot-path* :include '("src/main/**"))
    (let ((path "src/main/java/io/spring/application/CursorPager.java"))
      (is (equal
            "BOOLEAN"
            ;;                                                               ↓
            ;; public CursorPager(List<T> data, Direction direction, boolean hasExtra) {
            (find-fq-class-name
              (find-ast-in-ctx `((:path . ,path) (:line . 12) (:offset . 65)))
              path))))))

(test find-fq-class-name-for-inner-class-parameter
  (with-fixture jvm-ctx (*spring-boot-path* :include '("src/main/**"))
    (let ((path "src/main/java/io/spring/application/CursorPager.java"))
      (is (equal
            "io.spring.application.CursorPager$Direction"
            ;;                                            ↓
            ;; public CursorPager(List<T> data, Direction direction, boolean hasExtra) {
            (find-fq-class-name
              (find-ast-in-ctx `((:path . ,path) (:line . 12) (:offset . 46)))
              path))))))

(test get-scoped-index-paths-with-module-private
  (with-fixture jvm-ctx (*lightrun-path*)
    (is (equal
          '("api-service/src/main/java/com/baeldung/apiservice/adapters/http/TasksController.java")
          (inga/analyzer::get-scoped-index-paths
            '((:type . :module-private)
              (:path . "api-service/src/main/java/com/baeldung/apiservice/adapters/http/TasksController.java")
              (:fq-name . "com.baeldung.apiservice.adapters.http.TasksController.getUser-java.lang.String"))
            *index*)))))

(test get-scoped-index-paths-with-module-public
  (with-fixture jvm-ctx (*lightrun-path*)
    (is (null
          (find-if-not
            (lambda (p) (uiop:string-prefix-p "api-service" p))
            (inga/analyzer::get-scoped-index-paths
              '((:type . :module-public)
                (:path . "api-service/src/main/java/com/baeldung/apiservice/adapters/users/UserRepository.java")
                (:fq-name . "com.baeldung.apiservice.adapters.users.UserRepository.getUserById-java.lang.String"))
              *index*))))))

(test not-matches-signature-with-no-args
  (with-fixture jvm-ctx (*lightrun-path*)
    (is (eq
          nil
          ;; https://spring.pleiades.io/spring-framework/docs/current/javadoc-api/org/springframework/web/util/UriComponentsBuilder.html
          (inga/analyzer/base::matches-signature
            "build"
            "build-boolean"
            *index*)))))

(test matches-signature-with-sub-class
  (with-fixture jvm-ctx (*spring-boot-path* :include '("src/main/**"))
    (is (eq
          t
          (inga/analyzer/base::matches-signature
            "io.spring.application.CursorPager.CursorPager-java.util.ArrayList-io.spring.application.CursorPager$Direction-BOOLEAN"
            "io.spring.application.CursorPager.CursorPager-java.util.List-io.spring.application.CursorPager$Direction-BOOLEAN"
            "src/main/java/io/spring/application/ArticleQueryService.java")))))

(test matches-signature-with-object-array
  (with-fixture jvm-ctx (*guava-modules*)
    (is (eq
          t
          (inga/analyzer/base::matches-signature
            "com.google.common.collect.Lists.newArrayList-java.lang.String-java.lang.String"
            "com.google.common.collect.Lists.newArrayList-java.lang.Object[]"
            "guava-io/src/test/java/com/baeldung/guava/GuavaIOUnitTest.java")))))

(test find-class-hierarchy-with-standard-class
  (with-fixture jvm-ctx (*lightrun-path*)
    (is (equal
          '("java.io.Serializable"
            "java.lang.Comparable"
            "java.lang.CharSequence"
            "java.lang.constant.Constable"
            "java.lang.constant.ConstantDesc"
            "java.lang.Object"
            "java.lang.String")
          (find-class-hierarchy
            "java.lang.String"
            "users-service/src/main/java/com/baeldung/usersservice/service/UsersService.java")))))

