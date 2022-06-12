(defpackage #:inga/test/jsx
  (:use #:cl
        #:fiveam
        #:inga/jsx))
(in-package #:inga/test/jsx)

(def-suite jsx)
(in-suite jsx)

(test can-inject-mark-when-the-opening-tag-is-single-line
  (is (equal
        "<button style={{outline: '2px dashed red', outlineOffset: '2px'}} onClick={() => {}}>"
        (inga/jsx:inject-mark-on-line
          "<button onClick={() => {}}>"
          2 1))))

(test can-inject-mark-when-the-opening-tag-is-line-breaking
  (is (equal
        "<button style={{outline: '2px dashed red', outlineOffset: '2px'}}"
        (inga/jsx:inject-mark-on-line
          "<button"
          2 1))))

(test does-not-inject-mark-when-not-tags
  (is (equal
        "const number = 1;"
        (inga/jsx:inject-mark-on-line
          "const number = 1;"
          7 1))))

