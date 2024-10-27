(defpackage #:inga/test/yaml
  (:use #:cl
        #:fiveam
        #:inga/yaml))
(in-package #:inga/test/yaml)

(def-suite yaml)
(in-suite yaml)

(test parse-yaml-with-single-server
  (is (equal
        `((:servers
            ((:clients
               ((:path . "b")))
             (:path . "a"))))
        (parse-yaml "servers:
                       - path: a
                         clients:
                           - path: b"))))

(test parse-yaml-with-multiple-servers
  (is (equal
        `((:servers
            ((:clients
               ((:path . "b")))
             (:path . "a"))
            ((:clients
               ((:path . "d")))
             (:path . "c"))))
        (parse-yaml "servers:
                       - path: a
                         clients:
                           - path: b
                       - path: c
                         clients:
                           - path: d"))))

(test parse-yaml-with-empty-clients
  (is (equal
        `((:servers
            ((:path . "a"))))
        (parse-yaml "servers:
                       - path: a"))))

(test parse-yaml-with-blank-servers
  (is (equal
        `((:servers))
        (parse-yaml " servers:"))))

