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

(test get-nodes-in-downward-direction
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
          (ast:extract ast '("B"))))))

(test get-nodes-in-upward-direction
  (let ((ast '(:obj
                ("type" . "A")
                ("children" . ((:obj
                                 ("type" . "B")
                                 ("children" . nil))
                               (:obj
                                 ("type" . "B")
                                 ("children" . nil)))))))
    (labels ((f (ast)
               (loop for node in (ast-value ast "children")
                     do
                     (setf (jsown:val node "parent") ast)
                     (f node))))
      (f ast))
    (setf ast (first (ast:extract ast '("B"))))
    (let ((actual (ast:extract ast '("A") :direction :upward)))
      (is (and
            (eq 1 (length actual))
            (equal
              "A"
              (ast-value (first actual) "type")))))))

(test get-nodes-with-asts
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
          (ast:extract (list ast) '("B"))))))

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
          (ast:extract ast '("B" "C"))))))

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
          (ast:extract ast '("*"))))))

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
          (ast-find-name nodes "a")))))

(test does-not-find-a-name
  (let ((nodes '((:obj
                   ("type" . "A")
                   ("name" . "a")
                   ("children" . nil)))))
    (is (equal
          nil
          (ast-find-name nodes "b")))))

(test does-not-contains-a-name
  (let ((nodes '((:obj
                   ("type" . "A")
                   ("children" . nil)))))
    (is (equal
          nil
          (ast-find-name nodes "a")))))

(test find-names
  (let ((nodes '((:obj
                   ("type" . "A")
                   ("name" . "a")
                   ("children" . nil))
                 (:obj
                   ("type" . "B")
                   ("name" . "b")
                   ("children" . nil)))))
    (is (equal
          '((:obj
              ("type" . "A")
              ("name" . "a")
              ("children" . nil))
            (:obj
              ("type" . "B")
              ("name" . "b")
              ("children" . nil)))
          (ast-find-names nodes '("a" "b"))))))

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
          (ast-find-suffix nodes "b")))))

(test does-not-find-a-suffix
  (let ((nodes '((:obj
                   ("type" . "A")
                   ("name" . "a.b")
                   ("children" . nil)))))
    (is (equal
          nil
          (ast-find-suffix nodes "c")))))

