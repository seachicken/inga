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
     :initarg :temp-path
     :accessor ast-index-disk-path)))

(defmethod initialize-instance :after ((ast-index ast-index-disk)
                                       &key (temp-path (merge-pathnames ".inga/")))
  (setf (slot-value ast-index 'index-path) (merge-pathnames "index/" temp-path)))

(defmethod create-indexes ((ast-index ast-index-disk) include include-files exclude)
  (ensure-directories-exist (ast-index-disk-path ast-index)) 
  (loop for path in (uiop:directory-files (format nil "~a/**/*" (ast-index-root-path ast-index)))
        do
        (let ((relative-path (enough-namestring path (ast-index-root-path ast-index))))
          (when (and (is-analysis-target relative-path include exclude)
                     (is-analysis-target relative-path include-files exclude))
            (setf (ast-index-paths ast-index)
                  (append (ast-index-paths ast-index) (list relative-path)))
            (handler-case
              (alexandria:write-string-into-file
                (format nil "~a" (parse (namestring path)))
                (get-index-path relative-path (ast-index-disk-path ast-index)))
              (error (e)
                     (format t "error: ~a, path: ~a~%" e path)
                     (stop-all-parsers)
                     (error 'inga-error)))))) 
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

