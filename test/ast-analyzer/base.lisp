(defpackage #:inga/test/ast-analyzer/base
  (:use #:cl
        #:fiveam
        #:inga/ast-analyzer))
(in-package #:inga/test/ast-analyzer/base)

(test get-value
  (let ((ast '(:obj
                ("type" . "A")
                ("name" . "a")
                ("children" . nil))))
    (is (equal
          "a"
          (ast-value ast "name")))))

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

(test get-nodes-by-wild-card
  (let ((ast '(:obj
                ("type" . "A")
                ("children" . ((:obj
                                 ("type" . "B")
                                 ("children" . nil))
                               (:obj
                                 ("type" . "C")
                                 ("children" . nil)))))))
    (is (equal
          '((:obj
              ("type" . "B")
              ("children" . nil))
            (:obj
              ("type" . "C")
              ("children" . nil)))
          (ast-get ast '("*"))))))

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

(test find-suffix
  (let ((nodes '((:obj
                   ("type" . "A")
                   ("name" . "a.b")
                   ("children" . nil)))))
    (is (equal
          '((:obj
              ("type" . "A")
              ("name" . "a.b")
              ("children" . nil)))
          (ast-find-suffix "b" nodes :key-name "fq")))))

(test does-not-find-a-suffix
  (let ((nodes '((:obj
                   ("type" . "A")
                   ("name" . "a.b")
                   ("children" . nil)))))
    (is (equal
          nil
          (ast-find-suffix "c" nodes)))))

