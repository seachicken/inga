(defpackage #:inga/test/ast-analyzer/kotlin
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer
        #:inga/ast-index
        #:inga/test/helper))
(in-package #:inga/test/ast-analyzer/kotlin)

(def-suite kotlin)
(in-suite kotlin)

(defparameter *kotlin-path* (merge-pathnames #p"test/fixtures/kotlin/"))

(test find-definitions-for-method
  (with-fixture jvm-context (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/PrimaryConstructorDefinition.kt")
             (:name . "method")
             (:fq-name . "p1.PrimaryConstructorDefinition.method")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/PrimaryConstructorDefinition.kt" *kotlin-path*)
                      '((:line . 4) (:offset . 5)))))) ;; FIXME: actual offset is 9
          (find-definitions (create-range "p1/PrimaryConstructorDefinition.kt" :line 4))))))

(test find-references-for-primary-constructor
  (with-fixture jvm-context (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/PrimaryConstructorReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/PrimaryConstructorReference.kt" *kotlin-path*)
                      '((:line . 7) (:offset . 11))))))
          (find-references
            '((:path . "p1/PrimaryConstructorHelper.kt")
              (:name . "method")
              (:fq-name . "p1.PrimaryConstructorHelper.method"))
            *index*)))))

(test find-references-for-fq-method
  (with-fixture jvm-context (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/FqMethodReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/FqMethodReference.kt" *kotlin-path*)
                      '((:line . 5) (:offset . 29))))))
          (find-references
            '((:path . "p1/FqMethodHelper.kt")
              (:name . "method")
              (:fq-name . "p1.FqMethodHelper.method"))
            *index*)))))

(test find-references-for-java-class
  (with-fixture jvm-context (*kotlin-path* 'ast-index-disk)
    (is (equal
          `(((:path . "p1/JavaReference.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/JavaReference.kt" *kotlin-path*)
                      '((:line . 7) (:offset . 11))))))
          (find-references
            '((:path . "p1/KotlinReference.java")
              (:name . "method")
              (:fq-name . "p1.KotlinReference.method"))
            *index*)))))

(test find-references-for-rest-client-get-method
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          `(((:path . "p1/client/ClientRestTemplate.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/client/ClientRestTemplate.kt" *kotlin-path*)
                      '((:line . 10) (:offset . 29)))))
            ((:path . "p1/client/ClientRestTemplate.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/client/ClientRestTemplate.kt" *kotlin-path*)
                      '((:line . 18) (:offset . 29))))))
          (find-references
            `((:type . :rest-server)
              (:host . "8080")
              (:path . "/path")
              (:name . "GET"))
            *index*)))))

(test find-references-for-rest-client-post-method
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (is (equal
          `(((:path . "p1/client/ClientRestTemplate.kt")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "p1/client/ClientRestTemplate.kt" *kotlin-path*)
                      '((:line . 22) (:offset . 29))))))
          (find-references
            `((:type . :rest-server)
              (:host . "8080")
              (:path . "/path")
              (:name . "POST"))
            *index*)))))

(test find-fq-name-for-reference-with-string-literal
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/client/ClientRestTemplate.kt"))
      (is (equal
            "org.springframework.web.client.RestTemplate.getForObject-java.lang.String-java.lang.Class"
            (inga/ast-analyzer/kotlin::find-fq-name-for-reference
              ;;                     ↓
              ;; return restTemplate.getForObject("http://localhost:8080/path", String::class.java)
              (find-ast path `((:line . 10) (:offset . 29)) *index* :key-offset "textOffset")
              path
              *index*))))))

(test find-fq-name-for-reference-with-enum
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/client/ClientRestTemplate.kt"))
      (is (equal
            "org.springframework.web.client.RestTemplate.exchange-java.lang.String-org.springframework.http.HttpMethod-NULL-java.lang.Class"
            (inga/ast-analyzer/kotlin::find-fq-name-for-reference
              ;;                     ↓
              ;; return restTemplate.exchange("http://localhost:8080/path", HttpMethod.GET, null, String::class.java)
              (find-ast path `((:line . 18) (:offset . 29)) *index* :key-offset "textOffset")
              path
              *index*))))))

(test find-fq-name-for-reference-with-new-class
  (with-fixture jvm-context (*kotlin-path* 'ast-index-memory)
    (let ((path "p1/client/ClientRestTemplate.kt"))
      (is (equal
            "org.springframework.web.client.RestTemplate.postForObject-java.lang.String-java.lang.String-java.lang.Class"
            (inga/ast-analyzer/kotlin::find-fq-name-for-reference
              ;;                     ↓
              ;; return restTemplate.postForObject(
              (find-ast path `((:line . 22) (:offset . 29)) *index* :key-offset "textOffset")
              path
              *index*))))))

