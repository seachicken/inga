(defpackage #:inga/analyzer/base
  (:use #:cl
        #:inga/utils)
  (:import-from #:alexandria
                #:switch)
  (:import-from #:jsown)
  (:import-from #:inga/cache
                #:defunc)
  (:import-from #:inga/contexts
                #:context-ast-index
                #:context-exclude
                #:context-include
                #:context-kind
                #:context-lc)
  (:import-from #:inga/file
                #:get-file-type
                #:is-analysis-target)
  (:import-from #:inga/errors
                #:inga-error)
  (:import-from #:inga/ast-index
                #:ast-index-paths
                #:clean-indexes
                #:create-indexes
                #:get-ast) 
  (:import-from #:inga/language-client
                #:references-client)
  (:import-from #:inga/logger
                #:log-debug)
  (:export #:analyzer
           #:signature-load-failed
           #:*analyzers*
           #:*file-index*
           #:*rest-client-apis*
           #:analyzer-path
           #:analyzer-index
           #:start-analyzer
           #:stop-analyzer
           #:get-scoped-index-paths
           #:get-scoped-index-paths-generic
           #:set-index-group
           #:analyze
           #:find-definitions
           #:find-definitions-generic
           #:find-entrypoint
           #:find-entrypoint-generic
           #:find-references
           #:find-reference
           #:find-rest-clients
           #:find-fq-name
           #:find-fq-name-generic
           #:find-fq-class-name
           #:find-fq-class-name-generic
           #:find-reference-to-literal
           #:find-reference-to-literal-generic
           #:find-caller
           #:find-caller-generic
           #:find-signature
           #:find-class-hierarchy
           #:find-class-hierarchy-generic
           #:contains-offset
           #:ast-value
           #:get-asts
           #:filter-by-name
           #:filter-by-names
           #:ast-find-suffix
           #:find-ast
           #:debug-ast))
(in-package #:inga/analyzer/base)

(defparameter *index-path* (uiop:merge-pathnames* #p"inga_temp/"))
(defparameter *analyzers* nil)
(defparameter *results* (make-hash-table))
(defparameter *file-index* (make-hash-table))
(defparameter *rest-client-apis* (make-hash-table))

(defclass analyzer ()
  ((path
     :initarg :path
     :accessor analyzer-path)
   (index
     :initarg :index
     :accessor analyzer-index)))

(define-condition signature-load-failed ()
  ((path
     :initarg :path
     :reader signature-load-failed-path)
   (fq-class-name
     :initarg :fq-class-name
     :reader signature-load-failed-fq-class-name)))

(defgeneric start-analyzer (kind include exclude path index)
  (:method (kind include exclude path index)
   (error (format nil "unknown analyzer. kind: ~a" kind))))

(defgeneric stop-analyzer (analyzer)
  (:method (analyzer)))

(defmethod stop-analyzer :after (analyzer)
  (clrhash *file-index*)
  (clrhash *rest-client-apis*)
  (clrhash *results*))

(defun get-scoped-index-paths (pos index)
  (let ((analyzer (get-analyzer (cdr (assoc :path (or (cdr (assoc :file-pos pos)) pos))))))
    (if analyzer
        (get-scoped-index-paths-generic analyzer pos)
        (ast-index-paths index))))
(defgeneric get-scoped-index-paths-generic (analyzer pos)
  (:method (analyzer pos)
   (ast-index-paths (analyzer-index analyzer))))

(defgeneric set-index-group (analyzer path)
  (:method (analyzer path)))

(defun analyze (ctx ranges &key (success (lambda (results))) (failure (lambda (failures))))
  (labels ((flattern-results (sort-keys)
             (mapcan (lambda (k)
                       (copy-list (cdr (assoc :results (gethash k *results*)))))
                     sort-keys))
           (flattern-failures (sort-keys)
             (mapcan (lambda (k)
                       (copy-list (cdr (assoc :failures (gethash k *results*)))))
                     sort-keys))
           (handle-search (searching-results def)
             (if (gethash (sxhash def) *results*)
                 (setf (cdr (assoc :results (gethash (sxhash def) *results*)))
                       (list searching-results))
                 (setf (gethash (sxhash def) *results*)
                       `((:results . ,(list searching-results))
                         (:failures))))))
    (let* ((defs (remove-duplicates
                   (mapcan (lambda (r) (find-definitions r))
                           (remove-if-not (lambda (r)
                                            (is-analysis-target (context-kind ctx)
                                                                (cdr (assoc :path r))
                                                                (context-include ctx)
                                                                (context-exclude ctx)))
                                          ranges))
                   :test #'equal))
           (def-keys (mapcar (lambda (d) (sxhash d)) defs)))
      (maphash (lambda (k v)
                 (when (not (member k def-keys))
                   (remhash k *results*)))
               *results*)
      (loop for def in defs
        with threads
        unless (and (gethash (sxhash def) *results*)
                    (not (cdr (assoc :failures (gethash (sxhash def) *results*)))))
        do
        (when (cdr (assoc :failures (gethash (sxhash def) *results*)))
          (evictc-analyze-by-definition ctx def))
        (funcall success (flattern-results def-keys))
        (let ((def (copy-list def)))
          (push (sb-thread:make-thread
                  (lambda ()
                    (handler-bind ((signature-load-failed
                                     (lambda (s)
                                       (push s (cdr (assoc :failures
                                                           (gethash (sxhash def) *results*))))
                                       (funcall failure (flattern-failures def-keys)))))
                      (setf (gethash (sxhash def) *results*)
                            `((:results
                                ,(remove-if (lambda (r)
                                              (equal (cdr (assoc :type r)) "searching"))
                                            (analyze-by-definition
                                              ctx def
                                              (lambda (searching-results)
                                                (handle-search searching-results def)
                                                (funcall success (flattern-results def-keys))))))
                              (:failures)))
                      (funcall success (flattern-results def-keys)))))
                threads))
        finally (loop for thread in threads do (sb-thread:join-thread thread)))
      (mapcar (lambda (rs)
                (remove-if (lambda (r) (equal (cdr (assoc :type r)) "searching")) rs))
              (flattern-results def-keys)))))

(defunc analyze-by-definition (ctx def on-search)
  (loop
    with q = (make-queue)
    with results
    initially (enqueue q def)
    do
    (setf def (dequeue q))
    (unless def (return (remove-duplicates results :test #'equal)))

    (setf results (remove-if (lambda (r) (equal (cdr (assoc :type r)) "searching")) results))
    (funcall on-search (push `((:type . "searching")
                               (:origin . ,(if (assoc :origin def)
                                               (cdr (assoc :origin def))
                                               def))
                               ,@(when (assoc :origin def)
                                   `((:entrypoint . ,def))))
                             results))

    (unless (assoc :origin def)
      (push (cons :origin def) def))
    ;; node doesn't support fq-name
    (when (assoc :fq-name def)
      (if (assoc :visited-fq-names def)
          (adjoin (cdr (assoc :fq-name def)) (cdr (assoc :visited-fq-names def)))
          (push (cons :visited-fq-names (list (cdr (assoc :fq-name def)))) def)))
    (setf results (append results (find-entrypoints ctx def q)))))

(defun find-entrypoints (ctx pos q)
  (let ((refs (mapcan (lambda (ref)
                        (when (is-analysis-target (context-kind ctx) (cdr (assoc :path ref))
                                                  (context-include ctx) (context-exclude ctx))
                          (list ref)))
                      (if (context-lc ctx)
                          (references-client (context-lc ctx) pos) 
                          (find-references pos (context-ast-index ctx)))))
        results)
    (labels ((make-entrypoint (entrypoint)
               `((:type . "entrypoint")
                 (:origin . ,(if (cdr (assoc :origin pos))
                                 (cdr (assoc :origin pos))
                                 entrypoint))
                 (:entrypoint . ,entrypoint)))
             (make-connection (definition)
               `((:type . "connection")
                 (:origin . ,(cdr (assoc :file-pos pos)))
                 (:entrypoint . ,definition))))
      (when (eq (cdr (assoc :type pos)) :rest-server)
        (setf results (append results (list (make-entrypoint pos)))))
      (if refs
          (loop for ref in refs
            do
            (let ((entrypoint (find-entrypoint ref)))
              (if entrypoint
                  (setf results (append results (list (make-entrypoint entrypoint))))
                  (let ((def (first
                               (find-definitions
                                 `((:path . ,(cdr (assoc :path ref)))
                                   (:start-offset . ,(cdr (assoc :top-offset ref)))
                                   (:end-offset . ,(cdr (assoc :top-offset ref))))))))
                    (when (eq (cdr (assoc :type pos)) :rest-server)
                      (setf results (append results (list (make-connection def)))))
                    (if (member (cdr (assoc :fq-name def))
                                (cdr (assoc :visited-fq-names pos)) :test #'equal)
                        (setf results (append results (list (make-entrypoint def))))
                        (when def
                          (enqueue q (push (cons :origin
                                                 (if (eq (cdr (assoc :type pos)) :rest-server)
                                                     def
                                                     (cdr (assoc :origin pos))))
                                           def))))))))
          (setf results (append results (list (make-entrypoint pos))))))
    results))

(defun find-definitions (range)
  (funtime
    (lambda ()
      (let ((analyzer (get-analyzer (cdr (assoc :path range)))))
        (find-definitions-generic analyzer range)))
    :label "find-definitions"
    :args range))
(defgeneric find-definitions-generic (analyzer range))

(defun find-entrypoint (pos)
  (find-entrypoint-generic (cdr (assoc :typescript *analyzers*)) pos))
(defgeneric find-entrypoint-generic (analyzer pos)
  (:method (analyzer pos)))

(defun find-references (pos index)
  (unless pos
    (return-from find-references))

  (funtime
    (lambda ()
      (loop for path in (get-scoped-index-paths pos index)
            with results
            do
            (let* ((analyzer (get-analyzer path))
                   (ast (get-ast (analyzer-index analyzer) path))
                   (references (find-references-by-file analyzer path ast pos)))
              (when references
                (setf results (append results references))))
            finally (return results)))
    :label "find-references"
    :args pos))

(defun find-references-by-file (analyzer path ast target-pos)
  (let ((q (make-queue))
        results)
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (when (null ast) (return))

        (let* ((fq-name (find-fq-name ast path))
               (ref (when fq-name (find-reference analyzer target-pos fq-name ast path))))
          (when ref
            (setf results (append results (list ref)))))

        (when (jsown:keyp ast "children")
          (loop for child in (jsown:val ast "children")
                do (enqueue q child)))))
    results))

(defgeneric find-reference (analyzer target-pos fq-name ast path)
  (:method (analyzer target-pos fq-name ast path)
   (error (format nil "unknown analyzer. path: ~a" path))))

(defgeneric find-rest-clients (analyzer fq-name ast path)
  (:method (analyzer fq-name ast path)
   (error (format nil "unknown analyzer. path: ~a" path))))

(defmethod find-rest-clients :around (analyzer fq-name ast path)
  (funtime
    (lambda () (call-next-method))
    :label "find-rest-clients"
    :args path))

(defun find-fq-name (ast path)
  (find-fq-name-generic (get-analyzer path) ast path))
(defgeneric find-fq-name-generic (analyzer ast path)
  (:method (analyzer ast path)
   (error (format nil "unknown analyzer. path: ~a" path))))

(defun find-fq-class-name (ast path)
  (find-fq-class-name-generic (get-analyzer path) ast path))
(defgeneric find-fq-class-name-generic (analyzer ast path))

(defun find-reference-to-literal (ast path)
  (find-reference-to-literal-generic (get-analyzer path) ast path))
(defgeneric find-reference-to-literal-generic (analyzer ast path)
  (:method (analyzer ast path)
   (error (format nil "unknown analyzer. path: ~a" path))))

(defun find-caller (fq-names ast path)
  (find-caller-generic (get-analyzer path) fq-names ast path))
(defgeneric find-caller-generic (analyzer fq-names ast path)
  (:method (analyzer fq-names ast path)))

(defun find-signature (fq-name find-signatures path)
  (when (or (null fq-name) (equal fq-name ""))
    (return-from find-signature))

  (let ((fq-class-name
          (format nil "~{~a~^.~}"
                  (butlast (split #\.
                                  (if (>= (length (split #\- fq-name)) 2)
                                      (first (butlast (split #\- fq-name)))
                                      fq-name))))))
    (when (uiop:string-suffix-p fq-class-name "[]")
      (setf fq-class-name (string-right-trim "[]" fq-class-name)))

    (loop for method in (funcall find-signatures fq-class-name)
          with target-is-array = (is-array fq-name)
          with matched-methods
          do
          (when (matches-signature fq-name (cdr (assoc :fq-name method)) path)
            (if (or (and target-is-array (is-array (cdr (assoc :fq-name method))))
                    (and (not target-is-array) (not (is-array (cdr (assoc :fq-name method))))))
                (push method matched-methods)  
                (setf matched-methods (append matched-methods (list method)))))
          finally
          (return (progn
                    (when (> (length matched-methods) 1)
                      (log-debug (format nil "matched multiple signatures.~%  fq-name: ~a~%  matched-methods: ~a" fq-name matched-methods)))
                    (first matched-methods))))))

(defun matches-signature (target-fq-name api-fq-name path)
  (let* ((split-target-fq-names (split #\- target-fq-name))
         (split-api-fq-names (split #\- api-fq-name))
         (target-fq-class-name (format nil "~{~a~^.~}"
                                       (butlast (split #\. (first split-target-fq-names)))))
         (api-fq-class-name (format nil "~{~a~^.~}"
                                    (butlast (split #\. (first split-api-fq-names)))))
         (target-method-name (first (last (split #\. (first split-target-fq-names)))))
         (api-method-name (first (last (split #\. (first split-api-fq-names)))))
         (target-arg-names (cdr split-target-fq-names))
         (api-arg-names (cdr split-api-fq-names)))
    (unless (equal target-method-name api-method-name)
      (return-from matches-signature))

    (unless (or (eq (length target-arg-names) (length api-arg-names))
                (and (> (length api-arg-names) 0)
                     (is-array (nth (1- (length api-arg-names)) api-arg-names))
                     (or
                       (<= (length api-arg-names) (length target-arg-names))
                       (eq (1- (length api-arg-names)) (length target-arg-names)))))
      (return-from matches-signature))

    (when (and (not (equal target-fq-class-name api-fq-class-name))
               (not (find-if (lambda (super-class-name) (equal super-class-name api-fq-class-name))
                             (find-class-hierarchy target-fq-class-name path))))
      (return-from matches-signature)) 

    (loop for target-arg-name in target-arg-names
          for i below (length target-arg-names)
          with array-arg
          do
          (when (and (< i (length api-arg-names)) (is-array (nth i api-arg-names)))
            (setf array-arg (subseq (nth i api-arg-names) 0 (- (length (nth i api-arg-names)) 2))))
          (unless (or (equal target-arg-name "NULL")
                      (find-if (lambda (super-class-name)
                                 (or
                                   (equal super-class-name (nth i api-arg-names))
                                   (equal super-class-name array-arg)))
                               (find-class-hierarchy target-arg-name path)))
            (return-from matches-signature))))
  t)

(defun is-array (name)
  (uiop:string-suffix-p name "[]"))

(defun find-class-hierarchy (fq-class-name path)
  (let* ((analyzer (get-analyzer (namestring path)))
         (ast (get-ast (analyzer-index analyzer) path))
         (class-hierarchy (find-class-hierarchy-generic analyzer fq-class-name ast path)))
    (if class-hierarchy
        (return-from find-class-hierarchy class-hierarchy)
        (list fq-class-name))))
(defgeneric find-class-hierarchy-generic (analyzer fq-class-name root-ast path)
  (:method (analyzer fq-class-name root-ast path)))

(defun get-references-key (pos)
  (if (eq (cdr (assoc :type pos)) :rest-server)
      (intern (format nil "refs-~a-~a-~a-~a"
                      (cdr (assoc :type pos))
                      (cdr (assoc :host pos))
                      (cdr (assoc :name pos))
                      (cdr (assoc :path pos)))
              :keyword)
      (intern (format nil "refs-~a-~a"
                      (cdr (assoc :path pos))
                      (cdr (assoc :top-offset pos)))
              :keyword)))

(defun get-analyzer (path)
  (let ((result (cdr (assoc (get-file-type path) *analyzers*))))
    (if result
      result
      (error (format nil "analyzer not found. path: ~a" path)))))

(defun contains-offset (a-start a-end b-start b-end)
  (and (<= a-start b-end) (>= a-end b-start)))

(defun ast-value (ast key)
  (and (jsown:keyp ast key)
       (jsown:val ast key)))

(defun get-asts (ast info-path
                     &key (direction :downward)
                     (key-type "type") (key-downward "children") (key-upward "parent"))
  (if (equal direction :horizontal)
      (loop for node in (remove-if (lambda (r) (equal r ast))
                                   (ast-value (ast-value ast key-upward) key-downward))
            with results
            do
            (when (or
                    (find "*" info-path :test #'equal)
                    (find (ast-value node key-type) info-path :test #'equal))
              (push node results))
            finally (return results))
      (loop for path in info-path
            with results = (if (jsown:keyp ast key-type) (list ast) ast)
            do
            (setf results
                  (loop for node in (switch (direction)
                                      (:downward
                                        (apply #'append
                                               (mapcar (lambda (r) (ast-value r key-downward)) results)))
                                      (:upward
                                        (list (ast-value (first results) key-upward))))
                        with nodes
                        do
                        (when (or
                                (equal path "*")
                                (equal path (ast-value node key-type)))
                          (setf nodes (append nodes (list node))))
                        finally (return nodes)))
            finally (return results))))

(defun filter-by-name (nodes name &key (key-name "name"))
  (loop for node in nodes
        with results
        do
        (when (equal (ast-value node key-name) name)
          (setf results (append results (list node))))
        finally (return results)))

(defun filter-by-names (nodes names &key (key-name "name"))
  (loop for node in nodes
        with results
        do
        (when (find-if (lambda (n) (equal (ast-value node key-name) n)) names)
          (setf results (append results (list node))))
        finally (return results)))

(defun ast-find-suffix (nodes suffix &key (key-name "name"))
  (loop for node in nodes
        with results
        do
        (when (and
                (jsown:keyp node key-name)
                (uiop:string-suffix-p (jsown:val node key-name) suffix))
          (setf results (append results (list node))))
        finally (return results)))

(defun find-ast (pos index &key (type nil))
  (loop
    with q = (make-queue)
    with path = (cdr (assoc :path pos))
    with key-offset = (let ((file-type (get-file-type path)))
                        (cond
                          ((eq file-type :java)
                           "pos")
                          ((eq file-type :kotlin)
                           "textOffset")
                          ((t (error (format nil "unexpected file type: ~a" file-type))))))
    with ast 
    initially (enqueue q (get-ast index path))
    do
    (setf ast (dequeue q))
    (when (or (null ast)
              (and
                (eq (ast-value ast key-offset) (cdr (assoc :top-offset pos)))
                (if type (equal type (ast-value ast "type")) t)))
      (return ast))

    (loop for child in (ast-value ast "children") do (enqueue q child))))

(defun debug-ast (ast &key (key-downward "children") (key-upward "parent"))
  (format t "~&ast:~%") 
  (loop for key in (remove-if (lambda (key)
                                (or (equal key key-downward) (equal key key-upward)))
                              (jsown:keywords ast))
        do (format t " (~a . ~a)~%" key (ast-value ast key)))
  ast)

