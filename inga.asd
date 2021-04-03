(defsystem "inga"
  :version "0.1.0"
  :author "Seito Tanaka"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "inga/tests"))))

(defsystem "inga/tests"
  :author "Seito Tanaka"
  :license "MIT"
  :depends-on ("inga"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for inga"
  :perform (test-op (op c) (symbol-call :fiveam '#:run-all-tests)))
