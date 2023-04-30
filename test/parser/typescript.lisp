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
(test find-affected-pos-for-variable-object-literal-expression
  (let ((parser (make-parser :typescript *nestjs-path* *cache*)))
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

;; const NewTodoTextInput: React.FC = () => {
;;            ↓[out]
;;   function addTodo(e: React.KeyboardEvent<HTMLInputElement>): void {
;;     if (textInput.current === null) return ←[in]
;;   }
;; }
(test find-affected-pos-for-function
  (let ((parser (make-parser :typescript *react-path* *cache*)))
    (start-parser parser)
    (is (equal
          '((:path . "src/App/NewTodoInput/index.tsx")
            (:name . "addTodo") (:line . 12) (:offset . 12))
          (let ((src-path "src/App/NewTodoInput/index.tsx"))
            (inga/parser/typescript::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              13))))
    (stop-parser parser)))

;;              ↓[out]
;; export const User = createParamDecorator((data: any) => {
;;   const a = 0; ←[in]
;; });
(test find-affected-pos-for-variable-call-expression
  (let ((parser (make-parser :typescript *nestjs-path* *cache*)))
    (start-parser parser)
    (is (equal
          '((:path . "src/user/user.decorator.ts")
            (:name . "User") (:line . 5) (:offset . 14))
          (let ((src-path "src/user/user.decorator.ts"))
            (inga/parser/typescript::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              9))))
    (stop-parser parser)))

;;        ↓[out]
;; const [a, b] = f(
;;   list.forEach((a) => a) ←[in]
;; );
(test find-affected-pos-for-variable-call-expression-array
  (let ((parser (make-parser :typescript *fixtures-path* *cache*)))
    (start-parser parser)
    (is (equal
          '((:path . "declaration.ts")
            (:name . "a, b") (:line . 4) (:offset . 8))
          (let ((src-path "declaration.ts"))
            (inga/parser/typescript::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              5))))
    (stop-parser parser)))

;;       ↓[out]
;; const reverseCompleted = (id: Todo['id']): void => {
;;   const toggled: TodoListType = appState.todoList.map((t) => {
;;     return t ←[in]
;;   })
;; }
(test find-affected-pos-for-variable-arrow-function
  (let ((parser (make-parser :typescript *react-path* *cache*)))
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

;;       ↓[out]
;; const f2 = () => {
;;   return; ←[in]
;; };
(test find-affected-pos-for-variable-arrow-function-return-undefined
  (let ((parser (make-parser :typescript *fixtures-path* *cache*)))
    (start-parser parser)
    (is (equal
          '((:path . "declaration.ts")
            (:name . "f2") (:line . 8) (:offset . 7))
          (let ((src-path "declaration.ts"))
            (inga/parser/typescript::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              9))))
    (stop-parser parser)))

;; class ArticleService {
;;         ↓[out]
;;   async findAll(userId: number, query): Promise<any> {
;;     const andQueries = this.buildFindAllQuery(query); ←[in]
;;   }
;; }
(test find-affected-pos-for-method
  (let ((parser (make-parser :typescript *nestjs-path* *cache*)))
    (start-parser parser)
    (is (equal
          '((:path . "src/article/article.service.ts")
            (:name . "findAll") (:line . 45) (:offset . 9))
          (let ((src-path "src/article/article.service.ts"))
            (inga/parser/typescript::find-affected-pos
              parser
              src-path
              (exec-parser parser src-path)
              46))))
    (stop-parser parser)))

;; class ArticleService {
;;   async findAll(userId: number, query): Promise<any> {
;;     const andQueries = this.buildFindAllQuery(query);
;;   }
;; } ←[in]
(test ignore-affected-pos-when-end-block
  (let ((parser (make-parser :typescript *nestjs-path* *cache*)))
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

(test find-entrypoint
  (let ((parser (make-parser :typescript *react-path* *cache*)))
    (start-parser parser)
    (is (equal
          '((:path . "src/App/TodoList/Item/index.tsx")
            (:name . "input") (:line . 107) (:offset . 12))
          (inga/parser/typescript::find-entrypoint
            parser
            '((:path . "src/App/TodoList/Item/index.tsx")
              (:line . 111) (:offset . 29)))))
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
          "a" nil 1241))))

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
          "a" nil 328))))

