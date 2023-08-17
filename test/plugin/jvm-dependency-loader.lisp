(defpackage #:inga/test/plugin/jvm-dependency-loader
  (:use #:cl
        #:fiveam
        #:inga/plugin/jvm-dependency-loader))
(in-package #:inga/test/plugin/jvm-dependency-loader)

(def-suite java)
(in-suite java)

(test read-method
  (let ((process (start)))
    (load-project process "test/fixtures/spring-tutorials/lightrun/api-service")
    (is (equal
          '(:obj
             ("name" . "fromUriString")
             ("parameterTypes" "java.lang.String")
             ("returnType" . "org.springframework.web.util.UriComponentsBuilder"))
          (read-method
            process
            "org.springframework.web.util.UriComponentsBuilder.fromUriString-java.lang.String")))
    (stop process)))

