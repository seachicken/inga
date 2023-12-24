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
    (unless (symbol-call :fiveam '#:run-all-tests)
      (error "Tests failed"))))

(defsystem "inga/test-java"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/test/helper"
               "inga/test/main"
               "inga/test/traversal/java"
               "inga/test/traversal/kotlin"
               "inga/test/traversal/spring-java"
               "inga/test/traversal/spring-kotlin"
               "inga/test/plugin/jvm-helper"
               "inga/test/plugin/spring-property-loader"))

(defsystem "inga/test-typescript"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/test/helper"
               "inga/test/main"
               "inga/test/traversal/typescript"))

