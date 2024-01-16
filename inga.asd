(defsystem "inga"
  :author "Seito Tanaka"
  :license "MIT"
  :class :package-inferred-system
  :depends-on ("inga/all")
  :in-order-to ((test-op (test-op "inga/test"))))

(defsystem "inga/test"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/test/helper")
  :perform (test-op (o c)
                    (unless (uiop:symbol-call :fiveam '#:run-all-tests)
                      (error "Tests failed"))))

(defsystem "inga/test-jvm"
  :class :package-inferred-system
  :depends-on ("inga/test"
               "inga/test/main"
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
               "inga/test/main"
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

