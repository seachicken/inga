(defpackage #:inga/test/git
  (:use #:cl
        #:fiveam
        #:inga/git))
(in-package #:inga/test/git)

(def-suite git)
(in-suite git)

(defparameter *project-path* (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/"))

(test get-diff-of-one-line
  (is (equal '((("path" . "src/functions.ts") ("start" . 6) ("end" . 6)))
             (inga:get-diff *project-path* "a690a51" "dc33553"))))

(test get-diff-of-multiple-lines
  (is (equal '((("path" . "src/App/NewTodoInput/index.tsx") ("start" . 15) ("end" . 16))
               (("path" . "src/App/NewTodoInput/index.tsx") ("start" . 23) ("end" . 23)))
             (inga:get-diff *project-path* "a690a51" "4b0399f"))))

(test get-diff-of-multiple-files
  (is (equal '((("path" . "src/App/NewTodoInput/index.tsx") ("start" . 14) ("end" . 14))
               (("path" . "src/App/TodoList/index.tsx") ("start" . 16) ("end" . 16)))
             (inga:get-diff *project-path* "a690a51" "3a2d509"))))

(test not-get-diff-with-exclude
  (is (equal '()
             (inga:get-diff *project-path* "a690a51" "b4ac09c" '("*.test.tsx")))))
 
