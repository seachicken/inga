(defpackage #:inga/test/plugin/jvm-dependency-loader
  (:use #:cl
        #:fiveam
        #:inga/plugin/jvm-dependency-loader))
(in-package #:inga/test/plugin/jvm-dependency-loader)

(def-suite java)
(in-suite java)

(defparameter *lightrun-path*
  (truename (uiop:merge-pathnames* "test/fixtures/spring-tutorials/lightrun/")))

(test read-method
  (start *lightrun-path*)
  (is (equal
        '(:obj
           ("name" . "fromUriString")
           ("parameterTypes" . ((:obj ("name" . "java.lang.String") ("isInterface"))))
           ("returnType" . (:obj
                             ("name" . "org.springframework.web.util.UriComponentsBuilder")
                             ("isInterface"))))
        (read-method
          "org.springframework.web.util.UriComponentsBuilder.fromUriString-java.lang.String"
          "api-service/src/main/java/com/baeldung/apiservice/adapters/users/UserRepository.java")))
  (stop))

(test read-method-for-implements-class
  (start *lightrun-path*)
  (is (equal
        '(:obj
           ("name" . "path")
           ("parameterTypes" . ((:obj ("name" . "java.lang.String") ("isInterface"))))
           ("returnType" . (:obj
                             ("name" . "org.springframework.web.util.UriComponentsBuilder")
                             ("isInterface"))))
        (read-method
          "org.springframework.web.util.UriComponentsBuilder.path-java.lang.String"
          "api-service/src/main/java/com/baeldung/apiservice/adapters/users/UserRepository.java")))
  (stop))

