(defpackage #:inga/test/file
  (:use #:cl
        #:fiveam
        #:inga/file))
(in-package #:inga/test/file)

(def-suite file)
(in-suite file)

(test file-type-matches
  (is (equal
        t
        (is-match "src/index.js" :typescript))))

(test file-type-do-not-match
  (is (equal
        nil
        (is-match "src/index.ts" :java))))

(test analyze-when-the-file-name-does-not-match-exclude
  (is (equal
        t
        (is-analysis-target :typescript "src/index.js" '("*.js") '("*.test.js")))))

(test analyze-when-the-path-end-does-not-match-exclude
  (is (equal
        t
        (is-analysis-target :typescript "src/App.test.jsx" '("*.jsx") '("*.test.js")))))

(test not-analyze-when-the-path-matches-exclude
  (is (equal
        nil
        (is-analysis-target :typescript "src/App.test.js" '("*.js") '("*.test.js")))))

(test analyze-with-dir-and-file-include
  (is (equal
        t
        (is-analysis-target :typescript "app/index.js" '("app/**" "*.js")))))

(test not-analyze-with-dir-and-file-include
  (is (equal
        nil
        (is-analysis-target :typescript "app/pom.xml" '("app/**" "*.java")))))

(test not-analyze-when-the-file-extension-matches-exclude
  (is (equal
        nil
        (is-analysis-target :typescript "src/App.test.js" '("**/*.(js|jsx)") '("**/*.test.(js|jsx)"))))
  (is (equal
        nil
        (is-analysis-target :typescript "src/App.test.jsx" '("**/*.(js|jsx)") '("**/*.test.(js|jsx)")))))

(test not-analyze-when-the-parent-directory-matches-exclude
  (is (equal
        nil
        (is-analysis-target :typescript "App/test/App.test.js" '("*.js") '("App/test/*")))))

(test analyze-when-not-excluded
  (is (equal
        t
        (is-analysis-target :typescript "src/index.js" '("*.js")))))

(test analyze-with-multiple-include
  (is (equal
        t
        (is-analysis-target :typescript "src/index.js" '("*.js" "*.ts")))))

(test not-analyze-when-the-file-name-is-the-same-extension
  (is (equal
        nil
        (is-analysis-target :typescript "src/js" '("*.js")))))

(test analyze-without-include
  (is (equal
        t
        (is-analysis-target :typescript "src/index.js"))))

(test not-analyze-when-context-does-not-matched
  (is (equal
        nil
        (is-analysis-target :java "src/index.js"))))

