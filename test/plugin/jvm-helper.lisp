(defpackage #:inga/test/plugin/jvm-helper
  (:use #:cl
        #:fiveam
        #:inga/plugin/jvm-helper))
(in-package #:inga/test/plugin/jvm-helper)

(def-suite java)
(in-suite java)

(defparameter *lightrun-path* (merge-pathnames "test/fixtures/spring-tutorials/lightrun/"))
(defparameter *gradle-modules-path* (merge-pathnames "test/fixtures/spring-tutorials/gradle-modules/"))

(test find-base-path-for-maven
  (is (equal
        (merge-pathnames "api-service/" *lightrun-path*)
        (find-base-path "api-service/src/main/java/com/baeldung/apiservice/ApiServiceApplication.java" *lightrun-path*))))

(test find-base-path-for-gradle
  (is (equal
        (merge-pathnames "gradle-7/gradle-javadoc/" *gradle-modules-path*)
        (find-base-path "gradle-7/gradle-javadoc/src/main/java/com/baeldung/addition/Sum.java" *gradle-modules-path*))))

(test search-until-parent-root
  (is (equal
        nil
        (find-base-path "fixtures/declarations.kt" (merge-pathnames "test/")))))

