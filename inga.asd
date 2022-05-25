(defsystem "inga"
  :version "0.1.0"
  :author "Seito Tanaka"
  :license "MIT"
  :class :package-inferred-system
  :depends-on ("inga/all")
  :in-order-to ((test-op (test-op "inga/test"))))

(defsystem "inga/test"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/test/ts-helper"
               "inga/test/jsx"
               "inga/test/git"
               "inga/test/file")
  :perform (test-op (o c)
    (unless (symbol-call :fiveam '#:run-all-tests)
      (error "Tests failed"))))
