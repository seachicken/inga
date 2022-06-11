(defpackage #:inga/test/main
  (:use #:cl
        #:fiveam
        #:inga/main))
(in-package #:inga/test/main)

;; NOTE: To run this test file, execute `(asdf:test-system :inga)' in your Lisp.

(def-suite main)
(in-suite main)

(defparameter *project-path* (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/"))
(defparameter *build-path* (uiop:merge-pathnames* "test/fixtures/build/"))

(test analyze
  (inga:start)
  (is (equal
        '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10)))
        (inga:analyze (truename *project-path*) "a690a51" "4d33bd8")))
  (inga:stop))

(test analyze-with-exclude
  (inga:start)
  (is (equal
        nil
        (inga:analyze (truename *project-path*) "a690a51" "b4ac09c" '("*.test.tsx"))))
  (inga:stop))

(test analyze-by-range-in-arrow-function
  (inga:start)
  (is (equal
        '(((:path . "src/App/TodoList/Item/index.tsx") (:line . 107) (:offset . 12)))
        (inga/main::analyze-by-range
          (truename *project-path*)
          '(("path" . "src/App/TodoList/Item/index.tsx") ("start" . 65) ("end" . 65)))))
  (inga:stop))

(test analyze-by-range-in-function-declaration
  (inga:start)
  (is (equal
        '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10)))
        (inga/main::analyze-by-range
          (truename *project-path*)
          '(("path" . "src/App/NewTodoInput/index.tsx") ("start" . 14) ("end" . 14)))))
  (inga:stop))

(test analyze-by-range-in-external-function
  (inga:start)
  (is (equal
        '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10)))
        (inga/main::analyze-by-range
          (truename *project-path*)
          '(("path" . "src/functions.ts") ("start" . 2) ("end" . 2))   
          '("*.test.ts"))))
  (inga:stop))

(test find-components
  (inga:start)
  (is (equal
        '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10)))
        (inga:find-components
          *project-path*
          (truename (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *project-path*))
          '((:path . "src/App/NewTodoInput/index.tsx") (:line . 12) (:offset . 12))
          (inga/main::make-queue))))
  (inga:stop))

(test find-components-with-exclude
  (inga:start)
  (is (equal
        '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10))
          ((:path . "src/App/index.tsx") (:line . 34) (:offset . 10))) 
        (inga:find-components
          *project-path*
          (truename (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *project-path*))
          '((:path . "src/App/NewTodoInput/index.tsx") (:line . 7) (:offset . 7))
          (inga/main::make-queue)
          '("*.test.tsx"))))
  (inga:stop))

(test inject-mark
  (uiop:run-program (format nil "cp -r ~a ~a" *project-path* *build-path*))

  (inga:inject-mark
    *build-path*
    '(((:path . "src/App/NewTodoInput/index.tsx") (:line . 34) (:offset . 10))))
  (is (equal
        (uiop:read-file-string (uiop:merge-pathnames* "test/fixtures/index.tsx"))
        (uiop:read-file-string (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *build-path*))))
  
  (uiop:run-program (format nil "rm -r ~a" *build-path*)))

