(defsystem "inga"
  :author "Seito Tanaka"
  :license "MIT"
  :class :package-inferred-system
  :depends-on ("inga/all")
  :in-order-to ((test-op (test-op "inga/test"))))

(defsystem "inga/test"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/test/analyzer/base"
               "inga/test/config"
               "inga/test/file"
               "inga/test/helper"
               "inga/test/server"
               "inga/test/main"
               "inga/test/path")
  :perform (test-op (o c)
                    (unless (uiop:symbol-call
                              :fiveam '#:run!
                              (list
                                (uiop:find-symbol* '#:file
                                                   :inga/test/file)
                                (uiop:find-symbol* '#:server
                                                   :inga/test/server)
                                (uiop:find-symbol* '#:main
                                                   :inga/test/main)
                                (uiop:find-symbol* '#:path
                                                   :inga/test/path)
                                (uiop:find-symbol* '#:analyzer/base
                                                   :inga/test/analyzer/base)))
                      (error "Tests failed"))))

(defsystem "inga/test-jvm"
  :class :package-inferred-system
  :depends-on ("inga/test"
               "inga/test/analyzer/java"
               "inga/test/analyzer/kotlin"
               "inga/test/ast-index/disk"
               "inga/test/ast-index/memory"
               "inga/test/plugin/jvm-helper"
               "inga/test/plugin/spring/spring-property-loader"
               "inga/test/plugin/spring/analyzer/java"
               "inga/test/plugin/spring/analyzer/kotlin")
  :perform (test-op (o c)
                    (unless (uiop:symbol-call
                              :fiveam '#:run!
                              (list
                                (uiop:find-symbol* '#:java
                                                   :inga/test/ast-index/disk)
                                (uiop:find-symbol* '#:java
                                                   :inga/test/ast-index/memory)
                                (uiop:find-symbol* '#:java
                                                   :inga/test/analyzer/java)
                                (uiop:find-symbol* '#:kotlin
                                                   :inga/test/analyzer/kotlin)
                                (uiop:find-symbol* '#:java
                                                   :inga/test/plugin/jvm-helper)
                                (uiop:find-symbol* '#:java
                                                   :inga/test/plugin/spring/spring-property-loader)
                                (uiop:find-symbol* '#:java
                                                   :inga/test/plugin/spring/analyzer/java)
                                (uiop:find-symbol* '#:kotlin
                                                   :inga/test/plugin/spring/analyzer/kotlin)))
                      (error "Tests failed"))))

(defsystem "inga/test-node"
  :class :package-inferred-system
  :depends-on ("inga/test"
               "inga/test/analyzer/typescript")
  :perform (test-op (o c)
                    (unless (uiop:symbol-call
                              :fiveam '#:run!
                              (list
                                (uiop:find-symbol* '#:typescript
                                                   :inga/test/analyzer/typescript)))
                      (error "Tests failed"))))

