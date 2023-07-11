(defpackage #:inga/test/ast-analyzer/base
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer))
(in-package #:inga/test/ast-analyzer/base)

(test get-nodes
  (let ((ast '(:obj
                ("type" . "A")
                ("children" . ((:obj
                                 ("type" . "B")
                                 ("children" . nil))
                               (:obj
                                 ("type" . "B")
                                 ("children" . nil)))))))
    (is (equal
          '((:obj
              ("type" . "B")
              ("children" . nil))
            (:obj
              ("type" . "B")
              ("children" . nil)))
          (ast-get ast '("B"))))))

(test get-nested-nodes
  (let ((ast '(:obj
                ("type" . "A")
                ("children" . ((:obj
                                 ("type" . "B")
                                 ("children" . ((:obj
                                                  ("type" . "C")
                                                  ("children" . nil))))))))))
    (is (equal
          '((:obj
              ("type" . "C")
              ("children" . nil)))
          (ast-get ast '("B" "C"))))))

(test find-name
  (let ((nodes '((:obj
                   ("type" . "A")
                   ("name" . "a")
                   ("children" . nil)))))
    (is (equal
          '((:obj
              ("type" . "A")
              ("name" . "a")
              ("children" . nil)))
          (ast-find-name "a" nodes)))))

(test does-not-find-a-name
  (let ((nodes '((:obj
                   ("type" . "A")
                   ("name" . "a")
                   ("children" . nil)))))
    (is (equal
          nil
          (ast-find-name "b" nodes)))))

(test does-not-contains-a-name
  (let ((nodes '((:obj
                   ("type" . "A")
                   ("children" . nil)))))
    (is (equal
          nil
          (ast-find-name "a" nodes)))))

