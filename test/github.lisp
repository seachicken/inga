(defpackage #:inga/test/github
  (:use #:cl
        #:fiveam
        #:inga/github))
(in-package #:inga/test/github)

(def-suite github)
(in-suite github)

(test get-code-hierarchy
  (is (equal (format nil "~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~%"
                     "- 📂 b"
                     "  - 📂 a"
                     "    - 📄 [1.tsx - a](https://github.com/owner/repo/blob/sha/b/a/1.tsx#L1)"
                     "    - 📄 [1.tsx - b](https://github.com/owner/repo/blob/sha/b/a/1.tsx#L2)"
                     "  - 📄 [1.tsx - a](https://github.com/owner/repo/blob/sha/b/1.tsx#L1)"
                     "- 📂 c/a"
                     "  - 📄 [1.tsx - a](https://github.com/owner/repo/blob/sha/c/a/1.tsx#L3)"
                     "- 📄 [a.tsx - a](https://github.com/owner/repo/blob/sha/a.tsx#L4)")
             (inga/github::get-code-hierarchy
               "https://github.com/owner/repo/" "sha"
               '(((:path . "b/a/1.tsx") (:name . "a") (:line . 1))
                 ((:path . "b/a/1.tsx") (:name . "b") (:line . 2))
                 ((:path . "b/1.tsx") (:name . "a") (:line . 1))
                 ((:path . "c/a/1.tsx") (:name . "a") (:line . 3))
                 ((:path . "a.tsx") (:name . "a") (:line . 4)))))))

(test output-dirs-with-nest-nested-dirs
  (is (equal
        (format nil "~a~%~a~%"
                "- 📂 a/b"
                "  - 📂 c")
        (inga/github::output-dirs
          nil
          '("a" "b" "c")
          '("a" "b" "d" "e")))))

(test output-dirs-with-prev-nested-dirs
  (is (equal
        (format nil "~a~%"
                "  - 📂 d/e")
        (inga/github::output-dirs
          '("a" "b" "c")
          '("a" "b" "d" "e")
          nil))))

