(defpackage :inga/test/main
  (:use :cl
        :fiveam
        :inga/main))
(in-package :inga/test/main)

;; NOTE: To run this test file, execute `(asdf:test-system :inga)' in your Lisp.

(def-suite diff-to-pos)

(in-suite diff-to-pos)

(test 解析する
  (inga:start)
  (is (equal '((:pos ("line" . 34) ("offset" . 10)))
             (inga:analyze)))
  (inga:stop))

(def-suite find-components)

(in-suite find-components)

(test 影響するコンポーネントの位置を返す
  (inga:start)
  (is (equal '((:pos ("line" . 34) ("offset" . 10)))
             (inga:find-components
               '(:pos ("line" . 12) ("offset" . 12)))))
  (inga:stop))

(run! 'diff-to-pos)
(run! 'find-components)
