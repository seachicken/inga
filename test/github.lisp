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
                     "    - 📄 [1.text#L1](https://github.com/owner/repo/blob/sha/b/a/1.text#L1)"
                     "    - 📄 [1.text#L2](https://github.com/owner/repo/blob/sha/b/a/1.text#L2)"
                     "  - 📄 [1.text#L1](https://github.com/owner/repo/blob/sha/b/1.text#L1)"
                     "- 📂 c/a"
                     "  - 📄 [1.text#L3](https://github.com/owner/repo/blob/sha/c/a/1.text#L3)"
                     "- 📄 [a.text#L4](https://github.com/owner/repo/blob/sha/a.text#L4)")
             (inga/github::get-code-hierarchy
               "https://github.com/owner/repo/" "sha"
               '(((:path . "b/a/1.text") (:line . 1))
                 ((:path . "b/a/1.text") (:line . 2))
                 ((:path . "b/1.text") (:line . 1))
                 ((:path . "c/a/1.text") (:line . 3))
                 ((:path . "a.text") (:line . 4)))))))

