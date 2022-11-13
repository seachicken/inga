(defpackage #:inga/test/file
  (:use #:cl
        #:fiveam
        #:inga/file))
(in-package #:inga/test/file)

(def-suite file)
(in-suite file)

(test filename-matches
  (is (equal
        t
        (is-match "src/index.js" '("*.js")))))

(test filename-do-not-match
  (is (equal
        nil
        (is-match "src/index.ts" '("*.js")))))

(test analyze-when-the-file-name-does-not-match-exclude
  (is (equal
        t
        (is-analysis-target "src/index.js" '("*.js") '("*.test.js")))))

(test analyze-when-the-path-end-does-not-match-exclude
  (is (equal
        t
        (is-analysis-target "src/App.test.jsx" '("*.jsx") '("*.test.js")))))

(test not-analyze-when-the-path-matches-exclude
  (is (equal
        nil
        (is-analysis-target "src/App.test.js" '("*.js") '("*.test.js")))))

(test not-analyze-when-the-file-extension-matches-exclude
  (is (equal
        nil
        (is-analysis-target "src/App.test.js" '("**/*.(js|jsx)") '("**/*.test.(js|jsx)"))))
  (is (equal
        nil
        (is-analysis-target "src/App.test.jsx" '("**/*.(js|jsx)") '("**/*.test.(js|jsx)")))))

(test not-analyze-when-the-parent-directory-matches-exclude
  (is (equal
        nil
        (is-analysis-target "App/test/App.test.js" '("*.js") '("App/test/*")))))

(test analyze-when-not-excluded
  (is (equal
        t
        (is-analysis-target "src/index.js" '("*.js")))))

(test not-analyze-when-not-include
  (is (equal
        nil
        (is-analysis-target "src/index.js" '()))))

