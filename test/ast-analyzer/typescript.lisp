(defpackage #:inga/test/ast-analyzer/typescript
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer))
(in-package #:inga/test/ast-analyzer/typescript)

(def-suite typescript)
(in-suite typescript)

(defparameter *fixtures-path* (uiop:merge-pathnames* "test/fixtures/"))
(defparameter *react-path* (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/"))
(defparameter *nestjs-path* (uiop:merge-pathnames* "test/fixtures/nestjs-realworld-example-app-prisma/"))

;;       ↓[out]
;; const article = {
;;   title: "Hello" ←[in]
;; };
(test find-definitions-for-variable-object-literal-expression
  (let ((ast-analyzer (start-ast-analyzer :typescript nil *nestjs-path*)))
    (create-indexes *nestjs-path* :include inga/main::*include-typescript*)
    (is (equal
          `(((:path . "src/article/article.service.ts")
              (:name . "articleAuthorSelect")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/article/article.service.ts" *nestjs-path*)
                      '((:line . 7) (:offset . 7))))))
          (find-definitions
            `((:path . "src/article/article.service.ts")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames "src/article/article.service.ts" *nestjs-path*)
                       '((:line . 8) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames "src/article/article.service.ts" *nestjs-path*)
                       '((:line . 8) (:offset . -1))))))))
    (clean-indexes)
    (stop-ast-analyzer ast-analyzer)))

;; const NewTodoTextInput: React.FC = () => {
;;            ↓[out]
;;   function addTodo(e: React.KeyboardEvent<HTMLInputElement>): void {
;;     if (textInput.current === null) return ←[in]
;;   }
;; }
(test find-definitions-for-function
  (let ((ast-analyzer (start-ast-analyzer :typescript nil *react-path*)))
    (create-indexes *react-path* :include inga/main::*include-typescript*)
    (is (equal
          `(((:path . "src/App/NewTodoInput/index.tsx")
              (:name . "addTodo")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/App/NewTodoInput/index.tsx" *react-path*)
                      '((:line . 12) (:offset . 12))))))
          (find-definitions
            `((:path . "src/App/NewTodoInput/index.tsx")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames "src/App/NewTodoInput/index.tsx" *react-path*)
                       '((:line . 13) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames "src/App/NewTodoInput/index.tsx" *react-path*)
                       '((:line . 13) (:offset . -1))))))))
    (clean-indexes)
    (stop-ast-analyzer ast-analyzer)))

;;              ↓[out]
;; export const User = createParamDecorator((data: any) => {
;;   const a = 0; ←[in]
;; });
(test find-definitions-for-variable-call-expression
  (let ((ast-analyzer (start-ast-analyzer :typescript nil *nestjs-path*)))
    (create-indexes *nestjs-path* :include inga/main::*include-typescript*)
    (is (equal
          `(((:path . "src/user/user.decorator.ts")
              (:name . "User")  
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/user/user.decorator.ts" *nestjs-path*)
                      '((:line . 5) (:offset . 14))))))
          (find-definitions
            `((:path . "src/user/user.decorator.ts")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames "src/user/user.decorator.ts" *nestjs-path*)
                       '((:line . 9) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames "src/user/user.decorator.ts" *nestjs-path*)
                       '((:line . 9) (:offset . -1))))))))
    (clean-indexes)
    (stop-ast-analyzer ast-analyzer)))

;;        ↓[out]
;; const [a, b] = f(
;;   list.forEach((a) => a) ←[in]
;; );
(test find-definitions-for-variable-call-expression-array
  (if t
      (skip "heap exhausted in CI")
      (progn
        (let ((ast-analyzer (start-ast-analyzer :typescript nil *fixtures-path*)))
          (create-indexes *fixtures-path* :include inga/main::*include-typescript*)
          (is (equal
                `(((:path . "declaration.ts")
                   (:name . "a, b")
                   ,(cons :top-offset
                          (convert-to-top-offset
                            (merge-pathnames "declaration.ts" *fixtures-path*)
                            '((:line . 4) (:offset . 8))))))
                (find-definitions
                  `((:path . "declaration.ts")
                    ,(cons :start-offset
                           (convert-to-top-offset
                             (merge-pathnames "declaration.ts" *fixtures-path*)
                             '((:line . 5) (:offset . 0))))
                    ,(cons :end-offset
                           (convert-to-top-offset
                             (merge-pathnames "declaration.ts" *fixtures-path*)
                             '((:line . 5) (:offset . -1))))))))
          (clean-indexes)
          (stop-ast-analyzer ast-analyzer)))))

;;       ↓[out]
;; const reverseCompleted = (id: Todo['id']): void => {
;;   const toggled: TodoListType = appState.todoList.map((t) => {
;;     return t ←[in]
;;   })
;; }
(test find-definitions-for-variable-arrow-function
  (let ((ast-analyzer (start-ast-analyzer :typescript nil *react-path*)))
    (create-indexes *react-path* :include inga/main::*include-typescript*)
    (is (equal
          `(((:path . "src/App/TodoList/Item/index.tsx")
              (:name . "reverseCompleted")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/App/TodoList/Item/index.tsx" *react-path*)
                      '((:line . 62) (:offset . 9))))))
          (find-definitions
            `((:path . "src/App/TodoList/Item/index.tsx")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames "src/App/TodoList/Item/index.tsx" *react-path*)
                       '((:line . 65) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames "src/App/TodoList/Item/index.tsx" *react-path*)
                       '((:line . 65) (:offset . -1))))))))
    (clean-indexes)
    (stop-ast-analyzer ast-analyzer)))

;;       ↓[out]
;; const f2 = () => {
;;   return; ←[in]
;; };
(test find-definitions-for-variable-arrow-function-return-undefined
  (if t
      (skip "heap exhausted in CI")
      (progn
        (let ((ast-analyzer (start-ast-analyzer :typescript nil *fixtures-path*)))
          (create-indexes *fixtures-path* :include inga/main::*include-typescript*)
          (is (equal
                `(((:path . "declaration.ts")
                   (:name . "f2")
                   ,(cons :top-offset
                          (convert-to-top-offset
                            (merge-pathnames "declaration.ts" *fixtures-path*)
                            '((:line . 8) (:offset . 7))))))
                (find-definitions
                  `((:path . "declaration.ts")
                    ,(cons :start-offset
                           (convert-to-top-offset
                             (merge-pathnames "declaration.ts" *fixtures-path*)
                             '((:line . 9) (:offset . 0))))
                    ,(cons :end-offset
                           (convert-to-top-offset
                             (merge-pathnames "declaration.ts" *fixtures-path*)
                             '((:line . 9) (:offset . -1))))))))
          (clean-indexes)
          (stop-ast-analyzer ast-analyzer)))))

;; class ArticleService {
;;         ↓[out]
;;   async findAll(userId: number, query): Promise<any> {
;;     const andQueries = this.buildFindAllQuery(query); ←[in]
;;   }
;; }
(test find-definitions-for-method
  (let ((ast-analyzer (start-ast-analyzer :typescript nil *nestjs-path*)))
    (create-indexes *nestjs-path* :include inga/main::*include-typescript*)
    (is (equal
          `(((:path . "src/article/article.service.ts")
              (:name . "findAll")
             ,(cons :top-offset
                    (convert-to-top-offset
                      (merge-pathnames "src/article/article.service.ts" *nestjs-path*)
                      '((:line . 45) (:offset . 9))))))
          (find-definitions
            `((:path . "src/article/article.service.ts")
              ,(cons :start-offset
                     (convert-to-top-offset
                       (merge-pathnames "src/article/article.service.ts" *nestjs-path*)
                       '((:line . 46) (:offset . 0))))
              ,(cons :end-offset
                     (convert-to-top-offset
                       (merge-pathnames "src/article/article.service.ts" *nestjs-path*)
                       '((:line . 46) (:offset . -1))))))))
    (clean-indexes)
    (stop-ast-analyzer ast-analyzer)))

(test find-entrypoint
  (let ((ast-analyzer (start-ast-analyzer :typescript nil *react-path*)))
    (create-indexes *react-path* :include inga/main::*include-typescript*)
    (is (equal
          `((:path . "src/App/TodoList/Item/index.tsx")
            (:name . "input")
            ,(cons :top-offset
                   (convert-to-top-offset
                     (merge-pathnames "src/App/TodoList/Item/index.tsx" *react-path*)
                     '((:line . 107) (:offset . 12)))))
          (inga/ast-analyzer/typescript::find-entrypoint
            `((:path . "src/App/TodoList/Item/index.tsx")
              ,(cons :top-offset
                     (convert-to-top-offset
                       (merge-pathnames "src/App/TodoList/Item/index.tsx" *react-path*)
                       '((:line . 111) (:offset . 29))))))))
    (clean-indexes)
    (stop-ast-analyzer ast-analyzer)))

(test convert-tsserver-pos-to-tsparser-pos
  (is (equal
        1241
        (convert-to-top-offset
          (merge-pathnames "src/App/NewTodoInput/index.tsx" *react-path*)
          '((:line . 39) (:offset . 69))))))

(test convert-tsparser-pos-to-tsserver-pos
  (is (equal
        '((:line . 39) (:offset . 69))
        (convert-to-pos
          (merge-pathnames "src/App/NewTodoInput/index.tsx" *react-path*)
          1241))))

(test convert-tsserver-pos-to-tsparser-pos-with-offset1
  (is (equal
        328
        (convert-to-top-offset
          (merge-pathnames "src/dataStructure.ts" *react-path*)
          '((:line . 21) (:offset . 1))))))

(test convert-tsparser-pos-to-tsserver-pos-with-offset1
  (is (equal
        '((:line . 21) (:offset . 1))
        (convert-to-pos
          (merge-pathnames "src/dataStructure.ts" *react-path*)
          328))))

