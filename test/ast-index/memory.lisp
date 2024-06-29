(defpackage #:inga/test/ast-index/memory
  (:use #:cl
        #:fiveam
        #:inga/ast-index))
(in-package #:inga/test/ast-index/memory)

(def-suite java)
(in-suite java)

(defparameter *java-path* (merge-pathnames "test/fixtures/general/"))

(test get-all-paths
  (let ((index (make-instance 'ast-index-memory
                              :root-path *java-path*)))
    (create-indexes index :java nil '("*.java") nil)
    (is (< 0 (length (ast-index-paths index))))
    (clean-indexes index)))

(test get-ast
  (let ((index (make-instance 'ast-index-memory
                              :root-path *java-path*)))
    (create-indexes index :java nil '("*.java") nil)
    (is (not (equal
               nil
               (get-ast index "src/main/java/p1/ConstructorDefinition.java"))))
    (clean-indexes index)))

