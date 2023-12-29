(defpackage #:inga/test/path
  (:use #:cl
        #:fiveam
        #:inga/path))
(in-package #:inga/test/path)

(def-suite path)
(in-suite path)

(test merge-paths
  (is (equal
        "/a"
        (merge-paths "/" "a"))))

(test merge-paths-with-combined-slashes
  (is (equal
        "/a"
        (merge-paths "/" "/a"))))

(test merge-paths-with-slash-added
  (is (equal
        "/a/b"
        (merge-paths "/a" "b"))))

(test get-variable-names
  (is (equal
        '("a" "b")
        (get-variable-names "/{a}/{b}"))))

(test replace-variable-name
  (is (equal
        "/{string}"
        (replace-variable-name "/{a}" "a" "string"))))

(test replace-variable-name-with-unmatched-variable
  (is (equal
        "/{string}/{b}"
        (replace-variable-name "/{a}/{b}" "a" "string"))))

