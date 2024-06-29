(defpackage #:inga/test/ast-index/disk
  (:use #:cl
        #:fiveam
        #:inga/ast-index))
(in-package #:inga/test/ast-index/disk)

(def-suite java)
(in-suite java)

(defparameter *java-path* (merge-pathnames "test/fixtures/general/"))

(test create-indexes-with-specific-files
  (let ((index (make-instance 'ast-index-disk
                              :root-path *java-path*)))
    (create-indexes index :java '("p1/**") '("*.java") nil)
    (is (null
          (remove-if
            (lambda (p) (uiop:string-suffix-p p ".java"))
            (ast-index-paths index))))
    (clean-indexes index)))

(test update-index-when-file-changed
  (let ((index (make-instance 'ast-index-disk :root-path *java-path*))
        (path "src/main/java/p1/ConstructorDefinition.java"))
    (create-indexes index :java nil '("*.java") nil)
    (let ((create-sec (sb-posix:stat-mtime
                        (sb-posix:stat
                          (inga/ast-index/disk::get-index-path
                            path (inga/ast-index/disk::ast-index-disk-path index))))))
      (update-index index path)
      (is (eq (sb-posix:stat-mtime
                (sb-posix:stat
                  (inga/ast-index/disk::get-index-path
                    path (inga/ast-index/disk::ast-index-disk-path index))))
              create-sec)))
    (clean-indexes index)))

(test get-all-paths
  (let ((index (make-instance 'ast-index-disk
                              :root-path *java-path*)))
    (create-indexes index :java nil '("*.java") nil)
    (is (< 0 (length (ast-index-paths index))))
    (clean-indexes index)))

(test get-ast
  (let ((index (make-instance 'ast-index-disk
                              :root-path *java-path*)))
    (create-indexes index :java nil '("*.java") nil)
    (is (not (equal
               nil
               (get-ast index "src/main/java/p1/ConstructorDefinition.java"))))
    (clean-indexes index)))

