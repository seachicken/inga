(defsystem "inga"
  :version "0.1.0"
  :author "Seito Tanaka"
  :license "MIT"
  :class :package-inferred-system
  :depends-on ("inga/all")
  :in-order-to ((test-op (test-op "inga/tests"))))

(defsystem "inga/tests"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/tests/all")
  :perform (test-op (o c) (symbol-call :fiveam '#:run-all-tests)))
