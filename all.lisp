(uiop:define-package #:inga/all
  (:nicknames #:inga)
  (:use-reexport
    #:inga/ast-analyzer
    #:inga/ast-parser
    #:inga/ast-index
    #:inga/main
    #:inga/git
    #:inga/file
    #:inga/errors
    #:inga/logger))

