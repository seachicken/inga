(defpackage #:inga/ast-index/memory
  (:use #:cl
        #:inga/ast-index/base)
  (:import-from #:jsown)
  (:import-from #:inga/ast-parser
                #:parse
                #:stop-all-parsers)
  (:import-from #:inga/file
                #:is-analysis-target)
  (:export #:ast-index-memory))
(in-package #:inga/ast-index/memory)

(defclass ast-index-memory (ast-index)
  ())

(defmethod create-indexes (ast-index include exclude)
  (loop for path in (uiop:directory-files (format nil "~a/**/*" (ast-index-root-path ast-index)))
        do
        (let ((relative-path (enough-namestring path (ast-index-root-path ast-index))))
          (when (is-analysis-target relative-path include exclude)
            (push relative-path (ast-index-paths ast-index))))))

(defmethod clean-indexes (ast-index)
  (stop-all-parsers))

(defmethod get-ast (ast-index path)
  (attach-parent (jsown:parse (parse (merge-pathnames path (ast-index-root-path ast-index))))))

