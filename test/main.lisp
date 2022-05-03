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
          (list '(:path . "src/App/NewTodoInput/index.tsx")
                '(:line . 34) '(:offset . 10)))
        (inga:analyze (truename *project-path*) "a690a51" "69bf5cb")))
  (inga:stop))

(test find-components
  (inga:start)
  (is (equal
        (list
          (list '(:path . "src/App/NewTodoInput/index.tsx")
                '(:line . 34) '(:offset . 10)))
        (inga:find-components
          *project-path*
          (truename (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *project-path*))
          '(:pos ("line" . 12) ("offset" . 12)))))
  (inga:stop))

(test inject-mark
  (defparameter build-path (uiop:merge-pathnames* "test/fixtures/build/"))
  (uiop:run-program (format nil "cp -r ~a ~a" *project-path* build-path))

  (inga:inject-mark
    build-path
    (list
      (list '(:path . "src/App/NewTodoInput/index.tsx")
            '(:line . 34) '(:offset . 10))))
  (is (equal
        (uiop:read-file-string (uiop:merge-pathnames* "test/fixtures/index.tsx"))
        (uiop:read-file-string (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" build-path))))
  
  (uiop:run-program (format nil "rm -r ~a" build-path)))

