(defpackage :inga/test/main
  (:use :cl
        :fiveam
        :inga/main))
(in-package :inga/test/main)

;; NOTE: To run this test file, execute `(asdf:test-system :inga)' in your Lisp.

(def-suite main)

(in-suite main)

(defparameter *project-path* (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/"))

(test analyze
  (inga:start)
  (is (equal
        (list
          (list (cons :path (uiop:merge-pathnames*
                              "src/App/NewTodoInput/index.tsx" *project-path*))
                '(:line . 34) '(:offset . 10)))
        (inga:analyze (truename *project-path*) "a690a51" "69bf5cb")))
  (inga:stop))

(test find-components
  (inga:start)
  (is (equal
        (list
          (list (cons :path (uiop:merge-pathnames*
                              "src/App/NewTodoInput/index.tsx" *project-path*))
                '(:line . 34) '(:offset . 10)))
        (inga:find-components
          (truename (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *project-path*))
          '(:pos ("line" . 12) ("offset" . 12)))))
  (inga:stop))
