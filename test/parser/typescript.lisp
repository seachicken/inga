(defpackage #:inga/test/parser/typescript
  (:use #:cl
        #:fiveam
        #:inga/parser/typescript))
(in-package #:inga/test/parser/typescript)

(def-suite parser/typescript)
(in-suite parser/typescript)

(defparameter *project-path* (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/"))

(test convert-tsserver-pos-to-tsparser-pos
  (is (equal
        (list (cons :path (uiop:merge-pathnames*
                            "src/App/NewTodoInput/index.tsx" *project-path*))
              '(:pos . 1241))
        (inga/parser/typescript::convert-to-ast-pos
          (list (cons :path (uiop:merge-pathnames*
                              "src/App/NewTodoInput/index.tsx" *project-path*))
                '(:line . 39) '(:offset . 69))))))

(test convert-tsparser-pos-to-tsserver-pos
  (is (equal
        (list '(:path . "src/App/NewTodoInput/index.tsx")
              '(:line . 39) '(:offset . 69))
        (inga/parser/typescript::convert-to-pos
          *project-path*
          (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *project-path*)
          1241))))

