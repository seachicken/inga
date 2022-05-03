(defpackage :inga/test/ts-helper
  (:use :cl
        :fiveam
        :inga/ts-helper))
(in-package :inga/test/ts-helper)

(def-suite ts-helper)

(in-suite ts-helper)

(defparameter *project-path* (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/"))

(test get-pos
  (is (equal '(:pos ("line" . 1) ("offset" . 1))
             (get-pos '(:obj ("text" . "addTodo")
                              ("kind" . "function")
                              ("nameSpan" :obj
                                ("start" :obj ("line" . 1) ("offset" . 1))
                                ("end" :obj ("line" . 1) ("offset" . 8))))))))

(test return-true-when-the-line-contains-a-function
  (is-true (contains-line
             '(:obj ("text" . "addtodo")
                      ("kind" . "function")
                      ("spans"
                       (:obj ("start" :obj ("line" . 1) ("offset" . 1))
                        ("end" :obj ("line" . 3) ("offset" . 1)))))
             2)))

(test return-false-when-the-line-contains-a-module
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

(test convert-tsserver-pos-to-tsparser-pos
  (is (equal
        (list (cons :path (uiop:merge-pathnames*
                            "src/App/NewTodoInput/index.tsx" *project-path*))
              '(:pos . 1241))
        (convert-to-ast-pos
          (list (cons :path (uiop:merge-pathnames*
                              "src/App/NewTodoInput/index.tsx" *project-path*))
                '(:line . 39) '(:offset . 69))))))

(test convert-tsparser-pos-to-tsserver-pos
  (is (equal
        (list '(:path . "src/App/NewTodoInput/index.tsx")
              '(:line . 39) '(:offset . 69))
        (convert-to-pos
          *project-path*
          (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *project-path*)
          1241))))
