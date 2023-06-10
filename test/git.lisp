(defpackage #:inga/test/git
  (:use #:cl
        #:fiveam
        #:inga/git))
(in-package #:inga/test/git)

(def-suite git)
(in-suite git)

(defparameter *project-path* (uiop:merge-pathnames* "test/fixtures/react-typescript-todo/"))

(test get-diff-of-one-line
  (let ((diff 
          (uiop:run-program
            (format nil "(cd ~a && git diff ~a --unified=0)" *project-path* "dc33553")
            :output :string)))
    (is (equal '(((:path . "src/functions.ts") (:start . 6) (:end . 6)))
                (inga:get-diff (make-string-input-stream diff))))))

(test get-diff-of-multiple-lines
  (let ((diff 
          (uiop:run-program
            (format nil "(cd ~a && git diff ~a --unified=0)" *project-path* "4b0399f")
            :output :string)))
    (is (equal '(((:path . "src/App/NewTodoInput/index.tsx") (:start . 15) (:end . 16))
                 ((:path . "src/App/NewTodoInput/index.tsx") (:start . 23) (:end . 23)))
               (inga:get-diff (make-string-input-stream diff))))))

(test get-diff-of-multiple-files
  (let ((diff 
          (uiop:run-program
            (format nil "(cd ~a && git diff ~a --unified=0)" *project-path* "3a2d509")
            :output :string)))
    (is (equal '(((:path . "src/App/NewTodoInput/index.tsx") (:start . 14) (:end . 14))
                 ((:path . "src/App/TodoList/index.tsx") (:start . 16) (:end . 16)))
               (inga:get-diff (make-string-input-stream diff))))))
 
