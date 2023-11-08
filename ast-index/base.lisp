(defpackage #:inga/ast-index/base
  (:use #:cl
        #:inga/utils)
  (:import-from #:jsown)
  (:export #:ast-index
           #:ast-index-root-path
           #:ast-index-store
           #:create-indexes
           #:clean-indexes
           #:get-ast
           #:attach-parent))
(in-package #:inga/ast-index/base)

(defclass ast-index ()
  ((root-path
     :initarg :root-path
     :accessor ast-index-root-path)
   (store
     :accessor ast-index-store)))

(defgeneric create-indexes (ast-index include exclude))

(defgeneric clean-indexes (ast-index))

(defgeneric get-ast (ast-index path))

(defun attach-parent (ast)
  (loop
    with root = ast
    with q = (make-queue)
    initially (enqueue q ast)
    do
    (setf ast (dequeue q))
    (unless ast (return root))

    (when (jsown:keyp ast "children")
      (loop for child in (jsown:val ast "children")
            do
            (setf (jsown:val child "parent") ast)
            (enqueue q child)))))

