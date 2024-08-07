(defsystem "inga"
  :author "Seito Tanaka"
  :license "MIT"
  :class :package-inferred-system
  :depends-on ("inga/all")
  :in-order-to ((test-op (test-op "inga/test"))))

(defsystem "inga/test"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/test/file"
               "inga/test/helper"
               "inga/test/language-server"
               "inga/test/main"
               "inga/test/path"
               "inga/test/traversal/base")
  :perform (test-op (o c)
                    (unless (uiop:symbol-call
                              :fiveam '#:run!
                              (list
                                (uiop:find-symbol* '#:file
                                                   :inga/test/file)
                                (uiop:find-symbol* '#:language-server
                                                   :inga/test/language-server)
                                (uiop:find-symbol* '#:main
                                                   :inga/test/main)
                                (uiop:find-symbol* '#:path
                                                   :inga/test/path)
                                (uiop:find-symbol* '#:traversal/base
                                                   :inga/test/traversal/base)))
                      (error "Tests failed"))))

(defsystem "inga/test-jvm"
  :class :package-inferred-system
  :depends-on ("inga/test"
               "inga/test/ast-index/disk"
               "inga/test/ast-index/memory"
               "inga/test/traversal/java"
               "inga/test/traversal/kotlin"
               "inga/test/plugin/jvm-helper"
               "inga/test/plugin/spring/spring-property-loader"
               "inga/test/plugin/spring/traversal/java"
               "inga/test/plugin/spring/traversal/kotlin")
  :perform (test-op (o c)
                    (unless (uiop:symbol-call
                              :fiveam '#:run!
                              (list
                                (uiop:find-symbol* '#:java
                                                   :inga/test/main)
                                (uiop:find-symbol* '#:java
                                                   :inga/test/ast-index/disk)
                                (uiop:find-symbol* '#:java
                                                   :inga/test/ast-index/memory)
                                (uiop:find-symbol* '#:jdk21
                                                   :inga/test/traversal/java)
                                (uiop:find-symbol* '#:jdk17
                                                   :inga/test/traversal/java)
                                (uiop:find-symbol* '#:kotlin
                                                   :inga/test/traversal/kotlin)
                                (uiop:find-symbol* '#:java
                                                   :inga/test/plugin/jvm-helper)
                                (uiop:find-symbol* '#:java
                                                   :inga/test/plugin/spring/spring-property-loader)
                                (uiop:find-symbol* '#:java
                                                   :inga/test/plugin/spring/traversal/java)
                                (uiop:find-symbol* '#:kotlin
                                                   :inga/test/plugin/spring/traversal/kotlin)))
                      (error "Tests failed"))))

(defsystem "inga/test-node"
  :class :package-inferred-system
  :depends-on ("inga/test"
               "inga/test/traversal/typescript")
  :perform (test-op (o c)
                    (unless (uiop:symbol-call
                              :fiveam '#:run!
                              (list
                                (uiop:find-symbol* '#:typescript
                                                   :inga/test/main)
                                (uiop:find-symbol* '#:typescript
                                                   :inga/test/traversal/typescript)))
                      (error "Tests failed"))))

