(defsystem "inga"
  :author "Seito Tanaka"
  :license "MIT"
  :class :package-inferred-system
  :depends-on ("inga/all")
  :in-order-to ((test-op (test-op "inga/test"))))

(defsystem "inga/test"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/test/git"
               "inga/test/github"
               "inga/test/file")
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
               "inga/test/ast-analyzer/kotlin"))

