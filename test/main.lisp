(defpackage :inga/test/main
  (:use :cl
        :fiveam
        :inga/main))
(in-package :inga/test/main)

;; NOTE: To run this test file, execute `(asdf:test-system :inga)' in your Lisp.

(def-suite main)

(in-suite main)

(defparameter *project-path* "/Users/seito/.roswell/lisp/quicklisp/local-projects/inga/test/fixtures/react-typescript-todo")

(test 解析する
  (inga:start)
  (is (equal '((:pos ("line" . 34) ("offset" . 10)))
             (inga:analyze *project-path* "a690a51" "dc33553")))
  (inga:stop))

(test 影響するコンポーネントの位置を返す
  (inga:start)
  (is (equal '((:pos ("line" . 34) ("offset" . 10)))
             (inga:find-components
               '(:pos ("line" . 12) ("offset" . 12)))))
  (inga:stop))

(run! 'main)

