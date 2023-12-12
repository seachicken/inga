(defpackage #:inga/test/traversal/base
  (:use #:cl
        #:fiveam
        #:inga/traversal)
  (:import-from #:inga/ast-index
                #:attach-parent))
(in-package #:inga/test/traversal/base)

(def-suite traversal/base)
(in-suite traversal/base)

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
          (trav:get-asts ast '("B"))))))

(test get-nodes-in-upward-direction
  (let ((ast (attach-parent
               '(:obj
                  ("type" . "A")
                  ("children" . ((:obj
                                   ("type" . "B")
                                   ("children" . nil))
                                 (:obj
                                   ("type" . "B")
                                   ("children" . nil))))))))
    (setf ast (first (trav:get-asts ast '("B"))))
    (let ((actual (trav:get-asts ast '("A") :direction :upward)))
      (is (and
            (eq 1 (length actual))
            (equal
              "A"
              (ast-value (first actual) "type")))))))

(test get-nodes-in-horizontal-direction
  (let ((ast (attach-parent
               '(:obj
                  ("type" . "A")
                  ("children" . ((:obj
                                   ("type" . "B")
                                   ("name" . "b1")
                                   ("children" . nil))
                                 (:obj
                                   ("type" . "B")
                                   ("name" . "b2")
                                   ("children" . nil))))))))
    (setf ast (first (trav:get-asts ast '("B"))))
    (let ((actual (first (trav:get-asts ast '("B") :direction :horizontal))))
      (is (equal
            "b2"
            (ast-value actual "name"))))))

(test does-not-get-nodes-when-target-is-missing-in-horizontal-direction
  (let ((ast (attach-parent
               '(:obj
                  ("type" . "A")
                  ("children" . ((:obj
                                   ("type" . "B")
                                   ("name" . "b1")
                                   ("children" . nil))
                                 (:obj
                                   ("type" . "B")
                                   ("name" . "b2")
                                   ("children" . nil))))))))
    (setf ast (first (trav:get-asts ast '("B"))))
    (is (equal
          nil
          (trav:get-asts ast '("C") :direction :horizontal)))))

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
          (trav:get-asts (list ast) '("B"))))))

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
          (trav:get-asts ast '("B" "C"))))))

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
          (trav:get-asts ast '("*"))))))

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

