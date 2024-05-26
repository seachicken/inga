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

(defmethod create-indexes ((ast-index ast-index-memory) ctx-kind include include-files exclude)
  (loop for path in (uiop:directory-files (format nil "~a/**/*" (ast-index-root-path ast-index)))
        do
        (let ((relative-path (enough-namestring path (ast-index-root-path ast-index))))
          (when (and (is-analysis-target ctx-kind relative-path include exclude)
                     (is-analysis-target ctx-kind relative-path include-files exclude))
            (setf (ast-index-paths ast-index)
                  (append (ast-index-paths ast-index) (list relative-path)))))))

(defmethod update-index ((ast-index ast-index-memory) path))

(defmethod clean-indexes ((ast-index ast-index-memory))
  (stop-all-parsers))

(defmethod get-ast (ast-index path)
  (attach-parent (jsown:parse (parse (merge-pathnames path (ast-index-root-path ast-index))))))

