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

(defsystem "inga/test-typescript"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/test/main"
               "inga/test/ast-analyzer/typescript"))

(defsystem "inga/test-java"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/test/main"
               "inga/test/ast-analyzer/java"
               "inga/test/ast-analyzer/kotlin"
               "inga/test/plugin/jvm-helper"
               "inga/test/plugin/spring-property-loader"))

