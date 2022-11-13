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
               (inga/github::group-by-entorypoint
                 '(((:origin (:path . "a/1.ts") (:name . "a") (:line . 1))
                    (:entorypoint (:path . "b/a/1.tsx") (:name . "b") (:line . 2)))
                   ((:origin (:path . "a/1.ts") (:name . "b") (:line . 2))
                    (:entorypoint (:path . "b/a/1.tsx") (:name . "a") (:line . 1)))
                   ((:origin (:path . "a/1.ts") (:name . "c") (:line . 3))
                    (:entorypoint (:path . "b/a/1.tsx") (:name . "b") (:line . 2)))
                   ((:origin (:path . "a/1.ts") (:name . "b") (:line . 2))
                    (:entorypoint (:path . "b/1.tsx") (:name . "a") (:line . 1)))
                   ((:origin (:path . "a/1.ts") (:name . "b") (:line . 1))
                    (:entorypoint (:path . "c/a/1.tsx") (:name . "a") (:line . 3)))
                   ((:origin (:path . "a/1.ts") (:name . "b") (:line . 1))
                    (:entorypoint (:path . "a.tsx") (:name . "a") (:line . 4)))))))))

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

(test filter-by-key
  (is (equal
        '(((:origin (:path . "a/1.ts") (:name . "a") (:line . 1))
           (:entorypoint (:path . "b/a/1.tsx") (:name . "a") (:line . 1))))
        (inga/github::filter-by-key
          '(((:origin (:path . "a/1.ts") (:name . "a") (:line . 1))
             (:entorypoint (:path . "b/a/1.tsx") (:name . "a") (:line . 1)))
            ((:origin (:path . "a/1.ts") (:name . "b") (:line . 1))
             (:entorypoint (:path . "b/a/1.tsx") (:name . "a") (:line . 1))))
          :entorypoint))))

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

