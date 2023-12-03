(uiop:define-package #:inga/all
  (:nicknames #:inga)
  (:use-reexport
    #:inga/ast-parser
    #:inga/ast-index
    #:inga/errors
    #:inga/file
    #:inga/git
    #:inga/logger  
    #:inga/main
    #:inga/traversal))

