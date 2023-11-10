(defpackage #:inga/ast-index/disk
  (:use #:cl
        #:inga/ast-index/base)
  (:import-from #:alexandria)
  (:import-from #:jsown)
  (:import-from #:inga/ast-parser
                #:parse
                #:stop-all-parsers)
  (:import-from #:inga/file
                #:is-analysis-target)
  (:import-from #:inga/errors
                #:inga-error)
  (:export #:ast-index-disk))
(in-package #:inga/ast-index/disk)

(defclass ast-index-disk (ast-index)
  ((index-path
     :initform (merge-pathnames "inga_temp/")
     :accessor ast-index-disk-path)))

(defmethod create-indexes ((ast-index ast-index-disk) include exclude)
  (clean-indexes ast-index)
  (ensure-directories-exist (ast-index-disk-path ast-index)) 
  (loop for path in (uiop:directory-files (format nil "~a/**/*" (ast-index-root-path ast-index)))
        do
        (let ((relative-path (enough-namestring path (ast-index-root-path ast-index))))
          (when (is-analysis-target relative-path include exclude)
            (push relative-path (ast-index-paths ast-index))
            (handler-case
              (alexandria:write-string-into-file
                (format nil "~a" (parse (namestring path)))
                (get-index-path relative-path (ast-index-disk-path ast-index)))
              (error (e)
                     (format t "error: ~a, path: ~a~%" e path)
                     (stop-all-parsers)
                     (error 'inga-error)))))) 
  (setf (ast-index-paths ast-index) (reverse (ast-index-paths ast-index)))
  (stop-all-parsers))

(defmethod clean-indexes ((ast-index ast-index-disk))
  (uiop:delete-directory-tree (ast-index-disk-path ast-index)
                              :validate t
                              :if-does-not-exist :ignore))

(defmethod get-ast ((ast-index ast-index-disk) path)
  (attach-parent
    (jsown:parse (alexandria:read-file-into-string
                   (get-index-path path (ast-index-disk-path ast-index))))))

(defun get-index-path (original-path index-path)
  (merge-pathnames
    index-path
    (ppcre:regex-replace-all "/" original-path "--")))

