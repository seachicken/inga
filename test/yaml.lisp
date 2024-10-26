(defpackage #:inga/test/yaml
  (:use #:cl
        #:fiveam
        #:inga/yaml))
(in-package #:inga/test/yaml)

(def-suite yaml)
(in-suite yaml)

(test parse-yaml-with-single-server
  (is (equal
        `(:servers
           ((:clients
              ((:name . "b")))
            (:name . "a")))
        (parse-yaml "servers:
                       - name: a
                         clients:
                           - name: b"))))

(test parse-yaml-with-multiple-servers
  (is (equal
        `(:servers
           ((:clients
              ((:name . "b")))
            (:name . "a"))
           ((:clients
              ((:name . "d")))
            (:name . "c")))
        (parse-yaml "servers:
                       - name: a
                         clients:
                           - name: b
                       - name: c
                         clients:
                           - name: d"))))

(test parse-yaml-with-empty-clients
  (is (equal
        `(:servers
           ((:name . "a")))
        (parse-yaml "servers:
                       - name: a"))))

(test parse-yaml-with-blank-servers
  (is (equal
        `(:servers)
        (parse-yaml " servers:"))))

