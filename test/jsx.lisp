(defpackage :inga/test/jsx
  (:use :cl
        :fiveam
        :inga/jsx))
(in-package :inga/test/jsx)

(test can-inject-mark-when-the-opening-tag-is-single-line
  (is (equal
        "<button data-inga=\"1\" onClick={() => {}}>"
        (inga::inject-mark-on-line
          "<button onClick={() => {}}>"
          2 1))))

(test can-inject-mark-when-the-opening-tag-is-line-breaking
  (is (equal
        "<button data-inga=\"1\""
        (inga::inject-mark-on-line
          "<button"
          2 1))))

(test does-not-inject-mark-when-not-tags
  (is (equal
        "const number = 1;"
        (inga::inject-mark-on-line
          "const number = 1;"
          7 1))))

