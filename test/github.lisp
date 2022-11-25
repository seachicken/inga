(defpackage #:inga/test/github
  (:use #:cl
        #:fiveam
        #:inga/github))
(in-package #:inga/test/github)

(def-suite github)
(in-suite github)

(test group-by-entorypoint
  (is (equal
        '(((:entorypoint (:path . "b/a/1.ts") (:name . "a") (:line . 1))
           (:origins (((:path . "a/1.ts") (:name . "a") (:line . 1))
                      ((:path . "a/1.ts") (:name . "b") (:line . 1))))))
        (inga/github::group-by-entorypoint
          '(((:origin (:path . "a/1.ts") (:name . "a") (:line . 1))
             (:entorypoint (:path . "b/a/1.ts") (:name . "a") (:line . 1)))
            ((:origin (:path . "a/1.ts") (:name . "b") (:line . 1))
             (:entorypoint (:path . "b/a/1.ts") (:name . "a") (:line . 1))))))))

(test get-code-hierarchy
  (is (equal (format nil "~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~%"
                     "- 📂 b"
                     "  - 📂 a"
                     "    - 📂 d"
                     "      - 📄 [1.tsx - a](https://github.com/owner/repo/blob/sha/b/a/d/1.tsx#L1)"
                     "    - 📄 [1.tsx - a](https://github.com/owner/repo/blob/sha/b/a/1.tsx#L1)"
                     "    - 📄 [1.tsx - b](https://github.com/owner/repo/blob/sha/b/a/1.tsx#L2)"
                     "  - 📄 [1.tsx - a](https://github.com/owner/repo/blob/sha/b/1.tsx#L1)"
                     "- 📂 c/a"
                     "  - 📄 [1.tsx - a](https://github.com/owner/repo/blob/sha/c/a/1.tsx#L3)"
                     "- 📄 [a.tsx - a](https://github.com/owner/repo/blob/sha/a.tsx#L4)")
             (inga/github::get-code-hierarchy
               "https://github.com/owner/repo/" "sha"
               (inga/github::group-by-entorypoint
                 '(((:entorypoint (:path . "b/a/1.tsx") (:name . "a") (:line . 1)))
                   ((:entorypoint (:path . "b/a/1.tsx") (:name . "b") (:line . 2)))
                   ((:entorypoint (:path . "b/1.tsx") (:name . "a") (:line . 1)))
                   ((:entorypoint (:path . "c/a/1.tsx") (:name . "a") (:line . 3)))
                   ((:entorypoint (:path . "b/a/d/1.tsx") (:name . "a") (:line . 1)))
                   ((:entorypoint (:path . "a.tsx") (:name . "a") (:line . 4)))))))))

(test output-dirs-by-the-first-line
  (is (equal
        (list
          (format nil "~a~%~a~%"
                  "- 📂 a"
                  "  - 📂 b")
          2)
        (multiple-value-list
          (inga/github::output-dirs
            '((("a") ("b")))
            0)))))

(test output-dirs-by-the-second-line-with-already-output-dirs
  (is (equal
        (list
          ""
          2)
        (multiple-value-list
          (inga/github::output-dirs
            '((("a") ("b") ("c"))
              (("a") ("b")))
            1)))))

(test output-dirs-by-the-second-line-with-not-yet-output-dirs
  (is (equal
        (list
          (format nil "~a~%"
                  "    - 📂 d")
          3)
        (multiple-value-list
          (inga/github::output-dirs
            '((("a") ("b") ("c"))
              (("a") ("b") ("d")))
            1)))))

(test output-short-dirs-by-the-first-line
  (is (equal
        (list
          (format nil "~a~%"
                  "- 📂 a/b")
          1)
        (multiple-value-list
          (inga/github::output-dirs
            '((("a" "b"))
              (("a")))
            0)))))

(test output-short-dirs-by-the-second-line-with-already-output-dirs
  (is (equal
        (list
          ""
          1)
        (multiple-value-list
          (inga/github::output-dirs
            '((("a" "b") ("c"))
              (("a" "b")))
            1)))))

(test output-short-dirs-by-the-second-line-with-not-yet-output-dirs
  (is (equal
        (list
          (format nil "~a~%"
                  "  - 📂 d")
          2)
        (multiple-value-list
          (inga/github::output-dirs
            '((("a" "b") ("c"))
              (("a" "b") ("d")))
            1)))))

(test group-by-short-dirs
  (is (equal
        '((("a" "b") ("c") ("d"))
          (("a" "b") ("c"))
          (("a" "b"))
          (("e" "f" "g")))
        (inga/github::group-by-short-dirs
          '(("a" "b" "c" "d")
            ("a" "b" "c")
            ("a" "b")
            ("e" "f" "g"))))))

(test get-min-matching-index
  (is (equal
        2
        (inga/github::get-min-matching-index '(("a" "b" "c") ("a" "b") ("c" "a")) 0))))

(test get-min-matching-index-when-not-match
  (is (equal
        2
        (inga/github::get-min-matching-index '(("a") ("b" "c")) 1))))

(test get-min-matching-index-with-nil
  (is (equal
        0
        (inga/github::get-min-matching-index '(("a") nil) 1))))

(test delete-matched-sub-dirs
  (is (equal
        '(("b")
          nil
          ("c"))
        (inga/github::delete-matched-sub-dirs '(("a" "b") ("a") ("c")) '("a")))))

