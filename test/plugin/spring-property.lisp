(defpackage #:inga/test/plugin/spring-property
  (:use #:cl
        #:fiveam
        #:inga/plugin/spring-property))
(in-package #:inga/test/plugin/spring-property)

(def-suite java)
(in-suite java)

(test find-property-from-properties
  (let ((process (start)))
    (is (equal
          "file:extra.properties"
          (find-property
            process
            "spring.config.location"
            "test/fixtures/spring-tutorials/spring-boot-modules/spring-boot-properties/src/main/resources/application.properties")))
    (stop process)))

(test find-property-from-yaml
  (let ((process (start)))
    (is (equal
          "production"
          (find-property
            process
            "environment"
            "test/fixtures/spring-tutorials/spring-boot-modules/spring-boot-properties/src/main/resources/application.yml")))
    (stop process)))

