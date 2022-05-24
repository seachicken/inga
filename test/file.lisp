(defpackage #:inga/test/file
  (:use #:cl
        #:fiveam
        #:inga/file))
(in-package #:inga/test/file)

(def-suite file)
(in-suite file)

(test analyze-when-the-file-name-does-not-match-exclude
  (is (equal
        t
        (is-analysis-target "src/index.js" '("*.test.js")))))

(test analyze-when-the-path-end-does-not-match-exclude
  (is (equal
        t
        (is-analysis-target "src/App.test.jsx" '("*.test.js")))))

(test not-analyze-when-the-path-matches-exclude
  (is (equal
        nil
        (is-analysis-target "src/App.test.js" '("*.test.js")))))

(test analyze-when-not-excluded
  (is (equal
        t
        (is-analysis-target "src/index.js"))))

