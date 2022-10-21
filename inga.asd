(defsystem "inga"
  :author "Seito Tanaka"
  :license "MIT"
  :class :package-inferred-system
  :depends-on ("inga/all")
  :in-order-to ((test-op (test-op "inga/test"))))

(defsystem "inga/test"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/test/jsx"
               "inga/test/git"
               "inga/test/github"
               "inga/test/file")
  :perform (test-op (o c)
    (unless (symbol-call :fiveam '#:run-all-tests)
      (error "Tests failed"))))

(defsystem "inga-typescript"
  :class :package-inferred-system
  :depends-on ("inga/all")
  :in-order-to ((test-op (test-op "inga/test-typescript"))))

(defsystem "inga/test-typescript"
  :class :package-inferred-system
  :depends-on ("fiveam"
               "inga/test/parser/typescript")
  :perform (test-op (o c)
    (unless (symbol-call :fiveam '#:run!
                         (find-symbol* '#:typescript
                                         :inga/test/parser/typescript))
      (error "Tests failed"))))

