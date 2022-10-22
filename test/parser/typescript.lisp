(defpackage #:inga/test/parser/typescript
  (:use #:cl
        #:fiveam
        #:inga/parser))
(in-package #:inga/test/parser/typescript)

(def-suite typescript)
(in-suite typescript)

(defparameter *react-path* (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/"))
(defparameter *nestjs-path* (uiop:merge-pathnames* "test/fixtures/nestjs-realworld-example-app-prisma/"))

(test find-affected-pos-for-variable-object-literal-expression
  (let ((parser (make-parser :typescript *nestjs-path*)))
    (start-parser parser)
    (is (equal
          '((:path . "src/article/article.service.ts")
            (:name . "articleAuthorSelect") (:line . 7) (:offset . 7))
          (let ((src-path "src/article/article.service.ts"))
            (inga/parser/typescript::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              8))))
    (stop-parser parser)))

(test find-affected-pos-for-variable-arrow-function
  (let ((parser (make-parser :typescript *react-path*)))
    (start-parser parser)
    (is (equal
          '((:path . "src/App/TodoList/Item/index.tsx")
            (:name . "reverseCompleted") (:line . 62) (:offset . 9))
          (let ((src-path "src/App/TodoList/Item/index.tsx"))
            (inga/parser/typescript::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              65))))
    (stop-parser parser)))

(test find-affected-pos-for-method
  (let ((parser (make-parser :typescript *nestjs-path*)))
    (start-parser parser)
    (is (equal
          '((:path . "src/article/article.service.ts")
            (:name . "findAll") (:line . 45) (:offset . 3))
          (let ((src-path "src/article/article.service.ts"))
            (inga/parser/typescript::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              47))))
    (stop-parser parser)))

(test ignore-affected-pos-when-end-block
  (let ((parser (make-parser :typescript *nestjs-path*)))
    (start-parser parser)
    (is (equal
          nil
          (let ((src-path "src/article/article.service.ts"))
            (inga/parser/typescript::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              150))))
    (stop-parser parser)))

(test convert-tsserver-pos-to-tsparser-pos
  (is (equal
        (list (cons :path (uiop:merge-pathnames*
                            "src/App/NewTodoInput/index.tsx" *react-path*))
              '(:pos . 1241))
        (inga/parser/typescript::convert-to-ast-pos
          *react-path*
          (list '(:path . "src/App/NewTodoInput/index.tsx")
                '(:line . 39) '(:offset . 69))))))

(test convert-tsparser-pos-to-tsserver-pos
  (is (equal
        (list '(:path . "src/App/NewTodoInput/index.tsx")
              '(:name . "a") '(:line . 39) '(:offset . 69))
        (inga/parser/typescript::convert-to-pos
          *react-path*
          "src/App/NewTodoInput/index.tsx"
          "a" 1241))))

(test convert-tsserver-pos-to-tsparser-pos-with-offset1
  (is (equal
        (list (cons :path (uiop:merge-pathnames*
                            "src/dataStructure.ts" *react-path*))
              '(:pos . 328))
        (inga/parser/typescript::convert-to-ast-pos
          *react-path*
          (list '(:path . "src/dataStructure.ts")
                '(:line . 21) '(:offset . 1))))))

(test convert-tsparser-pos-to-tsserver-pos-with-offset1
  (is (equal
        (list '(:path . "src/dataStructure.ts")
              '(:name . "a") '(:line . 21) '(:offset . 1))
        (inga/parser/typescript::convert-to-pos
          *react-path*
          "src/dataStructure.ts"
          "a" 328))))

