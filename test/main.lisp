(defpackage :inga/test/main
  (:use :cl
        :fiveam
        :inga/main))
(in-package :inga/test/main)

;; NOTE: To run this test file, execute `(asdf:test-system :inga)' in your Lisp.

(def-suite main)

(in-suite main)

(defparameter *project-path* (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/"))

;;(run! '解析する)
(test 解析する
  (inga:start)
  (is (equal
        (list
          (list (cons :path (uiop:merge-pathnames*
                              "src/App/NewTodoInput/index.tsx" *project-path*))
                '(:line . 34) '(:offset . 10)))
        (inga:analyze (truename *project-path*) "a690a51" "4d33bd8")))
  (inga:stop))

;;(run! 'inside-tsx)
(test inside-tsx
  (inga:start)
  ;; TODO: コンポーネント内の編集はコンポーネントの位置を返すべき
  (is (equal nil
  ;;(is (equal '((:pos
  ;;               ("path" . "/Users/seito/.roswell/local-projects/inga/test/fixtures/react-typescript-todo/src/App/NewTodoInput/index.tsx")
  ;;               ("line" . 34) ("offset" . 10)))
             (inga:analyze *project-path* "a690a51" "48c5018")))
  (inga:stop))

;; TODO: 結果のNILを除けていない
;;(debug! 'reference-to-another-file)
;;(test reference-to-another-file
;;  (inga:start)
;;  (is (equal '(((:path . "/Users/seito/.roswell/local-projects/inga/test/fixtures/react-typescript-todo/src/App/NewTodoInput/index.tsx")
;;                (:line . 34) (:offset . 10))
;;               ((:path . "/Users/seito/.roswell/local-projects/inga/test/fixtures/react-typescript-todo/src/App/TodoList/Item/index.test.tsx")
;;                (:line . 39) (:offset . 44)))
;;             (inga:analyze *project-path* "a690a51" "291ab2d")))
;;  (inga:stop))

(run! '影響するコンポーネントの位置を返す)
(test 影響するコンポーネントの位置を返す
  (inga:start)
  (is (equal
        (list
          (list (cons :path (uiop:merge-pathnames*
                              "src/App/NewTodoInput/index.tsx" *project-path*))
                '(:line . 34) '(:offset . 10)))
        (inga:find-components
          (truename (uiop:merge-pathnames* "src/App/NewTodoInput/index.tsx" *project-path*))
          '(:pos ("line" . 12) ("offset" . 12)))))
  (inga:stop))

(run! 'main)

