(uiop:define-package #:inga/all
  (:nicknames #:inga)
  (:use-reexport
    #:inga/analyzer  
    #:inga/ast-parser
    #:inga/ast-index
    #:inga/cache
    #:inga/cli
    #:inga/contexts
    #:inga/errors
    #:inga/file
    #:inga/git
    #:inga/language-server
    #:inga/logger  
    #:inga/main
    #:inga/plugin/spring/analyzer))

