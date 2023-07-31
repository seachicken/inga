(defpackage #:inga/test/plugin/spring-property
  (:use #:cl
        #:fiveam
        #:inga/plugin/spring-property))
(in-package #:inga/test/plugin/spring-property)

(def-suite spring-property)
(in-suite spring-property)

(test find-property
  (let ((process (start)))
    (is (equal
          "production"
          (find-property
            process
            "environment"
            "test/fixtures/spring-tutorials/spring-boot-modules/spring-boot-properties/src/main/resources/application.yml")))
    (stop process)))

