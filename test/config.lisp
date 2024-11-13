(defpackage #:inga/test/config
  (:use #:cl
        #:fiveam
        #:inga/config))
(in-package #:inga/test/config)

(def-suite config)
(in-suite config)

(defparameter *spring-path* (merge-pathnames "test/fixtures/spring-tutorials/"))

(test get-server-config
  (is (equal
        '((:path . "lightrun/users-service/"))
        (get-server-config
          '((:servers .
             (((:path . "lightrun/users-service/"))
              ((:path . "lightrun/tasks-service/")))))
          '((:type . :rest-server)
            (:file-pos
              (:path . "lightrun/users-service/src/main/A.java")))
          *spring-path*))))

(test server-config-has-changed
  (is (equal
        t
        (config-has-changed
          '((:servers .
             (((:clients .
                (((:path . "lightrun/api-service/"))))
               (:path . "lightrun/users-service/")))))
          '((:servers .
             (((:path . "lightrun/users-service/")))))
          '((:type . :rest-server)
            (:file-pos
              (:path . "lightrun/users-service/src/main/A.java")))
          *spring-path*))))

(test server-config-has-not-changed
  (is (equal
        nil
        (config-has-changed
          '((:servers .
             (((:path . "lightrun/users-service/")))))
          '((:servers .
             (((:path . "lightrun/users-service/")))))
          '((:type . :rest-server)
            (:file-pos
              (:path . "lightrun/users-service/src/main/A.java")))
          *spring-path*))))

(test config-to-obj
  (is (equal
        '(:obj
           ("servers" .
            ((:obj
               ("path" . "lightrun/users-service/")
               ("clients" .
                ((:obj
                   ("path" . "lightrun/api-service/"))))))))
        (config-to-obj
          '((:servers .
             (((:path . "lightrun/users-service/")
               (:clients .
                (((:path . "lightrun/api-service/"))))))))))))

(test obj-to-config
  (is (equal
        '((:servers .
           (((:path . "lightrun/users-service/")
             (:clients .
              (((:path . "lightrun/api-service/"))))))))    
        (obj-to-config
          '(:obj
             ("servers" .
              ((:obj
                 ("path" . "lightrun/users-service/")
                 ("clients" .
                  ((:obj
                     ("path" . "lightrun/api-service/"))))))))))))

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

(test parse-yaml-with-undefined-clients
  (is (equal
        `((:servers
            ((:path . "a"))))
        (parse-yaml "servers:
                       - path: a"))))

(test parse-yaml-with-empty-clients
  (is (equal
        `((:servers
            ((:clients)
             (:path . "a"))
            ((:path . "b"))))
        (parse-yaml "servers:
                       - path: a
                         clients:
                       - path: b"))))

(test parse-yaml-with-blank-servers
  (is (equal
        `((:servers))
        (parse-yaml " servers:"))))

(test to-yaml
  (is (equal
"servers:
  - path: a
    clients:
      - path: b
"
        (to-yaml `((:servers
                     ((:clients
                        ((:path . "b")))
                      (:path . "a"))))))))

