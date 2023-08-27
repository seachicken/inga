(defpackage #:inga/test/plugin/spring-property-loader
  (:use #:cl
        #:fiveam
        #:inga/plugin/spring-property-loader))
(in-package #:inga/test/plugin/spring-property-loader)

(def-suite java)
(in-suite java)

(defparameter *spring-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-tutorials/spring-boot-modules/spring-boot-properties/")))

(test find-property-from-properties
  (start *spring-path*)
  (is (equal
        "file:extra.properties"
        (find-property
          "spring.config.location"
          "src/main/java/com/baeldung/buildproperties/Application.java")))
  (stop))

(test find-property-from-yaml
  (start *spring-path*)
  (is (equal
        "production"
        (find-property
          "environment"
          "src/main/java/com/baeldung/buildproperties/Application.java")))
  (stop))

