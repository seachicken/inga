(defpackage inga/tests/main
  (:use :cl
        :common-lisp
        :inga
        :fiveam))
(in-package :inga/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :inga)' in your Lisp.

(def-suite diff-to-pos)

(in-suite diff-to-pos)

(test 解析する
  (inga:start)
  (is (equal '(:pos ("line" . 11) ("offset" . 12)) (inga:analyze)))
  (inga:stop))

(test 検索結果から位置を抽出する
  (is (equal '(:pos ("line" . 1) ("offset" . 1))
             (inga::get-pos '(:obj ("text" . "addTodo")
                              ("kind" . "function")
                              ("nameSpan" :obj
                                ("start" :obj ("line" . 1) ("offset" . 1))
                                ("end" :obj ("line" . 1) ("offset" . 8))))))))

(test moduleは必ず行に含まれるので無視する
  (is-false (inga::contains-line
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
  (is-true (inga::contains-line
             '(:obj ("text" . "addtodo")
                      ("kind" . "function")
                      ("spans"
                       (:obj ("start" :obj ("line" . 1) ("offset" . 1))
                        ("end" :obj ("line" . 3) ("offset" . 1)))))
             2)))

(def-suite find-components)

(in-suite find-components)

(test 影響するコンポーネントの位置を返す
  (inga:start)
  (is (equal '((:pos ("line" . 35) ("offset" . 10)))
             (inga:find-components
               '(:pos ("line" . 11) ("offset" . 12)))))
  (inga:stop))

(run! 'find-components)

