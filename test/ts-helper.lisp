(defpackage :inga/test/ts-helper
  (:use :cl
        :fiveam
        :inga/ts-helper))
(in-package :inga/test/ts-helper)

(def-suite ts-helper)

(in-suite ts-helper)

(test 検索結果から位置を抽出する
  (is (equal '(:pos ("line" . 1) ("offset" . 1))
             (get-pos '(:obj ("text" . "addTodo")
                              ("kind" . "function")
                              ("nameSpan" :obj
                                ("start" :obj ("line" . 1) ("offset" . 1))
                                ("end" :obj ("line" . 1) ("offset" . 8))))))))

(test moduleは必ず行に含まれるので無視する
  (is-false (contains-line
             '(:obj ("text" . "\"index\"")
                    ("kind" . "module")
                    ("spans"
                     (:obj ("start" :obj ("line" . 1) ("offset" . 1))
                      ("end" :obj ("line" . 5) ("offset" . 1))))
                    ("childItems"
                     (:obj ("text" . "addTodo")
                      ("kind" . "function")
                      ("spans"
                       (:obj ("start" :obj ("line" . 1) ("offset" . 1))
                        ("end" :obj ("line" . 3) ("offset" . 1)))))))
             2)))

(test functionが含まれる行であればtrueを返す
  (is-true (contains-line
             '(:obj ("text" . "addtodo")
                      ("kind" . "function")
                      ("spans"
                       (:obj ("start" :obj ("line" . 1) ("offset" . 1))
                        ("end" :obj ("line" . 3) ("offset" . 1)))))
             2)))

;;(run! 'tsserverの位置情報からtsparserの位置情報に変換)
(test tsserverの位置情報からtsparserの位置情報に変換
  (is (equal '((:path . "/Users/seito/.roswell/local-projects/inga/test/fixtures/react-typescript-todo/src/App/NewTodoInput/index.tsx")
               (:pos . 1241))
             (convert-to-ast-pos
               '((:path . "/Users/seito/.roswell/local-projects/inga/test/fixtures/react-typescript-todo/src/App/NewTodoInput/index.tsx")
                 (:line . 39) (:offset . 69))))))

(run! 'tsparserの位置情報からtsserverの位置情報に変換)
(test tsparserの位置情報からtsserverの位置情報に変換
  (is (equal '((:path . "/Users/seito/.roswell/local-projects/inga/test/fixtures/react-typescript-todo/src/App/NewTodoInput/index.tsx")
               (:line . 39) (:offset . 69))
             (convert-to-pos
               "/Users/seito/.roswell/local-projects/inga/test/fixtures/react-typescript-todo/src/App/NewTodoInput/index.tsx"
               1241))))

(run! 'ts-helper)
