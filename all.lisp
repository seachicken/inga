(uiop:define-package #:inga/all
  (:nicknames #:inga)
  (:use-reexport
    #:inga/ast-parser
    #:inga/main
    #:inga/git
    #:inga/file
    #:inga/errors
    #:inga/logger))

