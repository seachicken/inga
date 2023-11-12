(defpackage #:inga/test/ast-index/disk
  (:use #:cl
        #:fiveam
        #:inga/ast-index))
(in-package #:inga/test/ast-index/disk)

(defparameter *java-path* (merge-pathnames "test/fixtures/java/"))

(test create-indexes-with-specific-files
  (let ((index (make-instance 'ast-index-disk
                              :root-path *java-path*)))
    (create-indexes index '("p1/**") inga/ast-analyzer/java::*include-java* nil)
    (is (null
          (remove-if
            (lambda (p) (uiop:string-suffix-p p ".java"))
            (ast-index-paths index))))
    (clean-indexes index)))

(test get-all-paths
  (let ((index (make-instance 'ast-index-disk
                              :root-path *java-path*)))
    (create-indexes index inga/main::*include-java* inga/ast-analyzer/java::*include-java* nil)
    (is (< 0 (length (ast-index-paths index))))
    (clean-indexes index)))

(test get-ast
  (let ((index (make-instance 'ast-index-disk
                              :root-path *java-path*)))
    (create-indexes index inga/main::*include-java* inga/ast-analyzer/java::*include-java* nil)
    (is (not (equal
               nil
               (get-ast index "p1/ConstructorDefinition.java"))))
    (clean-indexes index)))

