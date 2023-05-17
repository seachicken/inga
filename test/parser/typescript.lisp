(defpackage #:inga/test/parser/typescript
  (:use #:cl
        #:fiveam
        #:inga/parser))
(in-package #:inga/test/parser/typescript)

(def-suite typescript)
(in-suite typescript)

(defparameter *fixtures-path* (uiop:merge-pathnames* "test/fixtures/"))
(defparameter *react-path* (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/"))
(defparameter *nestjs-path* (uiop:merge-pathnames* "test/fixtures/nestjs-realworld-example-app-prisma/"))
(defparameter *cache* (inga/cache:make-cache 100))

;;       ↓[out]
;; const article = {
;;   title: "Hello" ←[in]
;; };
(test find-affected-poss-for-variable-object-literal-expression
  (let ((parser (make-parser :typescript *nestjs-path* *cache*)))
    (start-parser parser inga/main::*include-typescript* nil)
    (is (equal
          '(((:name . "articleAuthorSelect")
             (:path . "src/article/article.service.ts")
             (:line . 7) (:offset . 7)))
          (find-affected-poss
            parser
            '((:path . "src/article/article.service.ts")
              (:start . 8) (:end . 8)))))
    (stop-parser parser)))

;; const NewTodoTextInput: React.FC = () => {
;;            ↓[out]
;;   function addTodo(e: React.KeyboardEvent<HTMLInputElement>): void {
;;     if (textInput.current === null) return ←[in]
;;   }
;; }
(test find-affected-poss-for-function
  (let ((parser (make-parser :typescript *react-path* *cache*)))
    (start-parser parser inga/main::*include-typescript* nil)
    (is (equal
          '(((:name . "addTodo")
             (:path . "src/App/NewTodoInput/index.tsx")
             (:line . 12) (:offset . 12)))
          (find-affected-poss
            parser
            '((:path . "src/App/NewTodoInput/index.tsx")
              (:start . 13) (:end . 13)))))
    (stop-parser parser)))

;;              ↓[out]
;; export const User = createParamDecorator((data: any) => {
;;   const a = 0; ←[in]
;; });
(test find-affected-poss-for-variable-call-expression
  (let ((parser (make-parser :typescript *nestjs-path* *cache*)))
    (start-parser parser inga/main::*include-typescript* nil)
    (is (equal
          '(((:name . "User")
            (:path . "src/user/user.decorator.ts")
            (:line . 5) (:offset . 14)))
          (find-affected-poss
            parser
            '((:path . "src/user/user.decorator.ts")
              (:start . 9) (:end . 9)))))
    (stop-parser parser)))

;;        ↓[out]
;; const [a, b] = f(
;;   list.forEach((a) => a) ←[in]
;; );
(test find-affected-poss-for-variable-call-expression-array
  (let ((parser (make-parser :typescript *fixtures-path* *cache*)))
    (start-parser parser inga/main::*include-typescript* nil)
    (is (equal
          '(((:name . "a, b")
            (:path . "declaration.ts")
            (:line . 4) (:offset . 8)))
          (find-affected-poss
            parser
            '((:path . "declaration.ts")
              (:start . 5) (:end . 5)))))
    (stop-parser parser)))

;;       ↓[out]
;; const reverseCompleted = (id: Todo['id']): void => {
;;   const toggled: TodoListType = appState.todoList.map((t) => {
;;     return t ←[in]
;;   })
;; }
(test find-affected-poss-for-variable-arrow-function
  (let ((parser (make-parser :typescript *react-path* *cache*)))
    (start-parser parser inga/main::*include-typescript* nil)
    (is (equal
          '(((:name . "reverseCompleted")
             (:path . "src/App/TodoList/Item/index.tsx")
             (:line . 62) (:offset . 9)))
          (find-affected-poss
            parser
            '((:path . "src/App/TodoList/Item/index.tsx")
              (:start . 65) (:end . 65)))))
    (stop-parser parser)))

;;       ↓[out]
;; const f2 = () => {
;;   return; ←[in]
;; };
(test find-affected-poss-for-variable-arrow-function-return-undefined
  (let ((parser (make-parser :typescript *fixtures-path* *cache*)))
    (start-parser parser inga/main::*include-typescript* nil)
    (is (equal
          '(((:name . "f2")
             (:path . "declaration.ts")
             (:line . 8) (:offset . 7)))
          (find-affected-poss
            parser
            '((:path . "declaration.ts")
              (:start . 9) (:end . 9)))))
    (stop-parser parser)))

;; class ArticleService {
;;         ↓[out]
;;   async findAll(userId: number, query): Promise<any> {
;;     const andQueries = this.buildFindAllQuery(query); ←[in]
;;   }
;; }
(test find-affected-poss-for-method
  (let ((parser (make-parser :typescript *nestjs-path* *cache*)))
    (start-parser parser inga/main::*include-typescript* nil)
    (is (equal
          '(((:name . "findAll")
            (:path . "src/article/article.service.ts")
            (:line . 45) (:offset . 9)))
          (find-affected-poss
            parser
            '((:path . "src/article/article.service.ts")
              (:start . 46) (:end . 46)))))
    (stop-parser parser)))

(test find-entrypoint
  (let ((parser (make-parser :typescript *react-path* *cache*)))
    (start-parser parser inga/main::*include-typescript* nil)
    (is (equal
          '((:name . "input")
            (:path . "src/App/TodoList/Item/index.tsx")
            (:line . 107) (:offset . 12))
          (inga/parser/typescript::find-entrypoint
            parser
            '((:path . "src/App/TodoList/Item/index.tsx")
              (:line . 111) (:offset . 29)))))
    (stop-parser parser)))

(test convert-tsserver-pos-to-tsparser-pos
  (is (equal
        1241
        (inga/parser/typescript::convert-to-top-offset
          *react-path*
          "src/App/NewTodoInput/index.tsx"
          '((:line . 39) (:offset . 69))))))

(test convert-tsparser-pos-to-tsserver-pos
  (is (equal
        '((:line . 39) (:offset . 69))
        (inga/parser/typescript::convert-to-pos
          *react-path*
          "src/App/NewTodoInput/index.tsx"
          1241))))

(test convert-tsserver-pos-to-tsparser-pos-with-offset1
  (is (equal
        328
        (inga/parser/typescript::convert-to-top-offset
          *react-path*
          "src/dataStructure.ts"
          '((:line . 21) (:offset . 1))))))

(test convert-tsparser-pos-to-tsserver-pos-with-offset1
  (is (equal
        '((:line . 21) (:offset . 1))
        (inga/parser/typescript::convert-to-pos
          *react-path*
          "src/dataStructure.ts"
          328))))

