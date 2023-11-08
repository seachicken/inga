(defpackage #:inga/test/ast-index/memory
  (:use #:cl
        #:fiveam
        #:inga/ast-index))
(in-package #:inga/test/ast-index/memory)

(defparameter *java-path* (merge-pathnames "test/fixtures/java/"))

(test get-ast
  (let ((index (make-instance 'ast-index-memory
                              :root-path *java-path*)))
    (create-indexes index inga/main::*include-java* nil)
    (is (not (equal
               nil
               (get-ast index "p1/ConstructorDefinition.java"))))
    (clean-indexes index)))

