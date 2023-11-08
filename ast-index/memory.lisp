(defpackage #:inga/ast-index/memory
  (:use #:cl
        #:inga/ast-index/base)
  (:import-from #:jsown)
  (:import-from #:inga/ast-parser
                #:parse
                #:stop-all-parsers)
  (:export #:ast-index-memory))
(in-package #:inga/ast-index/memory)

(defclass ast-index-memory (ast-index)
  ())

(defmethod create-indexes (ast-index include exclude))

(defmethod clean-indexes (ast-index)
  (stop-all-parsers))

(defmethod get-ast (ast-index path)
  (attach-parent (jsown:parse (parse path))))

