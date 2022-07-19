(defpackage #:inga/main
  (:use #:cl
        #:inga/git
        #:inga/github
        #:inga/ts-helper
        #:inga/jsx
        #:inga/file)
  (:import-from #:jsown)
  (:import-from #:alexandria)
  (:export #:command
           #:start
           #:stop
           #:find-components
           #:inject-mark))
(in-package #:inga/main)

(defvar *deepest-item*)
(defvar *tsserver*)
(defvar *tsparser*)
(defvar *jdtls*)
(defvar *javaparser*)

(defun command (&rest argv)
  (destructuring-bind (&key front-path back-path sha-a sha-b exclude github-token inject-mark) (parse-argv argv)
    (start :front-path front-path :back-path back-path)

    (let (project-path pr hostname)
      (if back-path
        (setf project-path back-path)
        (setf project-path front-path))

      (when github-token
        (setf hostname (inga/git:get-hostname project-path))
        (inga/github:login hostname github-token)
        (setf pr (inga/github:get-pr project-path))
        (destructuring-bind (&key base-url owner-repo number base-ref-name head-sha last-report) pr
          (unless sha-a
            (inga/git:track-branch base-ref-name project-path)
            (setf sha-a base-ref-name))))

      (let ((results (if back-path
                         (analyze-for-back back-path sha-a sha-b exclude)
                         (analyze-for-front front-path sha-a sha-b exclude))))
        (format t "~A~%" results)
        (when pr
          (destructuring-bind (&key base-url owner-repo number base-ref-name head-sha last-report) pr
            (inga/github:send-pr-comment hostname base-url owner-repo number results project-path head-sha last-report)))
        (when inject-mark
          (inject-mark front-path results))))

    (stop :front-path front-path :back-path back-path)))

(defun parse-argv (argv)
  (loop with front-path = nil
        with back-path = nil
        with sha-a = nil
        with sha-b = nil
        with exclude = '()
        with github-token = nil
        with inject-mark = nil
        for option = (pop argv)
        while option
        do (alexandria:switch (option :test #'equal)
             ("--front-path"
              (setf front-path (pop argv)))
             ("--back-path"
              (setf back-path (pop argv)))
             ("--sha-a"
              (setf sha-a (pop argv)))
             ("--sha-b"
              (setf sha-b (pop argv)))
             ("--exclude"
              (setf exclude
                    (append exclude
                            (mapcar (lambda (exc)
                                      (string-trim '(#\Space) exc))
                                    (split #\,
                                           (pop argv))))))
             ("--github-token"
              (setf github-token (pop argv)))
             ("--inject-mark"
              (setf inject-mark t)))
        finally
          (return (append 
                    (when front-path
                      (list :front-path (truename (uiop:merge-pathnames* front-path))))
                    (when back-path
                      (list :back-path (truename (uiop:merge-pathnames* back-path))))
                    (list :sha-a sha-a)
                    (when sha-b
                      (list :sha-b sha-b))
                    (list :exclude exclude)
                    (when github-token
                      (list :github-token github-token))
                    (list :inject-mark inject-mark)))))

(defun split (div sequence)
  (let ((pos (position div sequence)))
    (if pos
        (list* (subseq sequence 0 pos)
               (split div (subseq sequence (1+ pos))))
        (list sequence))))

(defun start (&key front-path back-path)
  (when front-path
    (setf *tsserver-id* 0)
    (setq *tsserver*
          (uiop:launch-program
            "tsserver"
            :input :stream :output :stream)))
  (when back-path
    (setf *jdtls-id* 0)
    (setq *jdtls* 
          (uiop:launch-program
            (format nil "~a/libs/jdtls/bin/jdtls -data ~a/libs/jdtls/workspace"
                    (uiop:getenv "INGA_HOME") (uiop:getenv "INGA_HOME"))
            :input :stream :output :stream))
    (exec-jdtls-initialize back-path)))

(defun stop (&key front-path back-path)
  (when front-path
    (uiop:close-streams *tsserver*))
  (when back-path
    (uiop:close-streams *jdtls*)))

(defun start-tsparser ()
  (setq *tsparser* (uiop:launch-program "tsparser" :input :stream :output :stream)))

(defun stop-tsparser ()
  (uiop:close-streams *tsparser*))

(defun start-javaparser ()
  (setq *javaparser* (uiop:launch-program
                       (format nil "java -cp ~a/libs/javaparser.jar inga.Main"
                               (uiop:getenv "INGA_HOME"))
                       :input :stream :output :stream)))

(defun stop-javaparser ()
  (uiop:close-streams *javaparser*))

(defun get-val (list key)
  (loop for item in list do
        (when (equal (car item) key)
          (return-from get-val (cdr item)))))

(defun analyze-for-front (project-path sha-a sha-b &optional (exclude '()))
  (remove-duplicates
    (mapcan (lambda (range)
              (analyze-by-range project-path range exclude))
            (get-diff project-path sha-a sha-b exclude))
    :test #'equal))

(defun analyze-for-back (project-path sha-a sha-b &optional (exclude '()))
  (remove-duplicates
    (mapcan (lambda (range)
              (analyze-by-range-for-back project-path range exclude))
            (get-diff project-path sha-a sha-b exclude))
    :test #'equal))

(defun analyze-by-range (project-path range &optional (exclude '()))
  (let ((q (make-queue))
        (results '()))
    (enqueue q range)
    (loop
      (let ((range (dequeue q)))
        (if (null range) (return results))

        (let ((src-path (format nil "~A~A" project-path (get-val range "path")))
              ast)

          (start-tsparser)
          (let ((result (exec-tsparser src-path)))
            (when (> (length result) 0)
              (setf ast (cdr (jsown:parse result)))))
          (stop-tsparser)

          (setf results
                (append results
                        (mapcan (lambda (pos)
                                  (let ((comps (find-components project-path src-path pos q exclude)))
                                    comps))
                                (get-affected-item-poss ast project-path src-path range)))))))))

(defun analyze-by-range-for-back (project-path range &optional (exclude '()))
  (let ((q (make-queue))
        (results '()))
    (enqueue q range)
    (loop
      (let ((range (dequeue q)))
        (if (null range) (return results))

        (let ((src-path (format nil "~a~a" project-path (get-val range "path")))
              ast)

          (start-javaparser)
          (let ((result (exec-javaparser src-path)))
            (when (> (length result) 0)
              (setf ast (cdr (jsown:parse result)))))
          (stop-javaparser)

          (setf results
                (append results
                        (mapcan (lambda (pos)
                                  (let ((endpoints (find-endpoints project-path src-path pos q exclude)))
                                    endpoints))
                                (get-affected-item-poss-for-back ast project-path src-path range)))))))))

(defun get-affected-item-poss (ast project-path src-path range)
  (remove-duplicates
    (remove nil
            (mapcar (lambda (line-no)
                      (let ((item-pos
                              (find-affected-item-pos
                                ast
                                (cdr (assoc :pos (convert-to-ast-pos
                                                   (list
                                                     (cons :path src-path)
                                                     (cons :line line-no)
                                                     (cons :offset 0))))))))
                        (when item-pos
                          (convert-to-pos project-path src-path item-pos))))
                    (loop for line-no
                          from (get-val range "start")
                          to (get-val range "end")
                          collect line-no)))
    :test #'equal))

(defun get-affected-item-poss-for-back (ast project-path src-path range)
  (remove-duplicates
    (remove nil
            (mapcar (lambda (line-no)
                      (let ((item-pos
                              (find-affected-item-pos-for-back ast line-no)))
                        (when item-pos
                          (acons :path src-path item-pos))))
                    (loop for line-no
                          from (get-val range "start")
                          to (get-val range "end")
                          collect line-no)))
    :test #'equal))

(defun find-affected-item-pos (ast pos)
  (let ((q (make-queue)))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (and
                (jsown:keyp ast "kind") (= (jsown:val ast "kind") *variable-declaration*)
                (jsown:keyp ast "start") (<= (jsown:val ast "start") pos)
                (jsown:keyp ast "end") (> (jsown:val ast "end") pos))
          (let ((init (jsown:val ast "initializer")))
            (when (= (jsown:val init "kind") *arrow-function*)
              (if (equal (find-return-type init) *jsx-element*)
                  (enqueue q (jsown:val init "body"))
                  (return (jsown:val ast "start"))))))

        (when (and
                (jsown:keyp ast "kind") (= (jsown:val ast "kind") *function-declaration*)
                (jsown:keyp ast "start") (<= (jsown:val ast "start") pos)
                (jsown:keyp ast "end") (> (jsown:val ast "end") pos))
          (return (jsown:val ast "start")))

        (when (and
                (jsown:keyp ast "kind")
                (= (jsown:val ast "kind") *variable-statement*))
          (let ((dec-list (jsown:val (jsown:val ast "declarationList") "declarations")))
            (loop for d in dec-list do
                  (enqueue q (cdr d)))))

        (when (jsown:keyp ast "statements")
          (loop for s in (jsown:val ast "statements") do
                (enqueue q (cdr s))))))))

(defun find-affected-item-pos-for-back (ast pos)
  (let ((q (make-queue)))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (and
                (or
                  (string= (cdr (car ast)) "com.github.javaparser.ast.body.ConstructorDeclaration")
                  (string= (cdr (car ast)) "com.github.javaparser.ast.body.MethodDeclaration"))
                (<= (jsown:val (jsown:val ast "range") "beginLine") pos)
                (> (jsown:val (jsown:val ast "range") "endLine") pos))
          (return
            (list (cons :line (jsown:val (jsown:val ast "range") "beginLine"))
                  (cons :offset (jsown:val (jsown:val ast "range") "beginColumn")))))

        (when (jsown:keyp ast "types")
          (loop for type in (jsown:val ast "types") do
                (enqueue q (cdr type))))

        (when (jsown:keyp ast "members")
          (loop for member in (jsown:val ast "members") do
                (enqueue q (cdr member))))))))

(defun find-return-type (ast)
  (when (and
          (jsown:keyp ast "type")
          (= (jsown:val (jsown:val ast "type") "kind") *void-keyword*))
    (return-from find-return-type))

  (let ((q (make-queue)))
    (enqueue q ast)
    (loop
      (let ((ast (dequeue q)))
        (if (null ast) (return))

        (when (and (jsown:keyp ast "kind") (= (jsown:val ast "kind") *return-statement*))
          (when (= (jsown:val (jsown:val ast "expression") "kind") *parenthesized-expression*)
            (when 
              (= (jsown:val (jsown:val
                              (jsown:val ast "expression") "expression") "kind") *jsx-element*)
              (return *jsx-element*))))

        (when (jsown:keyp ast "statements")
          (loop for s in (jsown:val ast "statements") do
                (enqueue q (cdr s))))
        
        (when (jsown:keyp ast "body")
          (enqueue q (jsown:val ast "body")))))))

(defun find-components (root-path src-path pos q &optional (exclude '()))
  (setf *tsserver-id* (+ *tsserver-id* 1))
  (call (format nil "{\"seq\": ~a, \"command\": \"open\", \"arguments\": {\"file\": \"~a\"}}"
                *tsserver-id* src-path))
  (setf *tsserver-id* (+ *tsserver-id* 1))
  (let ((result (exec (format nil "{\"seq\": ~a, \"command\": \"references\", \"arguments\": {\"file\": \"~a\", \"line\": ~a, \"offset\": ~a}}"
                              *tsserver-id* src-path
                              (cdr (assoc :line pos)) (cdr (assoc :offset pos)))))
        poss)
    (let ((refposs
            (remove-if (lambda (p) (or (equal p pos)
                                       (not (is-analysis-target (namestring (cdr (assoc :path p))) exclude))))
                     (mapcar (lambda (r)
                               (list
                                 (cons :path (jsown:val r "file"))
                                 (cons :line (jsown:val (jsown:val r "start") "line"))
                                 (cons :offset (jsown:val (jsown:val r "start") "offset"))))
                             (jsown:val (jsown:val result "body") "refs")))))
      (setq poss (mapcar (lambda (p)
                           (convert-to-ast-pos p))
                         refposs)))

    (start-tsparser)
    (setq poss
          (remove nil
                  (mapcar
                    (lambda (p)
                      (let ((result (exec-tsparser (namestring (cdr (assoc :path p))))))
                        (when (> (length result) 0)
                          (acons :ast (cdr (jsown:parse result)) p))))
                    poss)))
    (stop-tsparser)

    (remove-duplicates
      (remove nil
              (mapcar (lambda (p)
                        (let ((path (cdr (assoc :path p)))
                              (pos (cdr (assoc :pos p)))
                              (ast (cdr (assoc :ast p))))
                          (setq *nearest-comp-pos* nil)
                          (let ((comp-pos (find-component ast pos)))
                            (if (null comp-pos)
                                (let ((range-pos (convert-to-pos root-path path pos)))
                                  (enqueue q (list
                                               (cons "path" (enough-namestring path root-path))
                                               (cons "start" (cdr (assoc :line range-pos)))
                                               (cons "end" (cdr (assoc :line range-pos)))))
                                  nil)
                                (convert-to-pos root-path path comp-pos)))))
                      poss))
      :test #'equal)))

(defun find-endpoints (root-path src-path pos q &optional (exclude '()))
  (setf *jdtls-id* (+ *jdtls-id* 1))
  (let ((refs (exec-jdtls (format nil "{\"jsonrpc\":\"2.0\",\"id\":~a,\"method\":\"textDocument/references\",\"params\":{\"textDocument\":{\"uri\":\"file://~a\"},\"position\":{\"line\":~a,\"character\":~a},\"context\":{\"includeDeclaration\":false}}}"
                                  *jdtls-id* src-path
                                  ;; the zero-based line and offset
                                  (- (cdr (assoc :line pos)) 1) (- (cdr (assoc :offset pos)) 1)))))
    (setf refs (mapcar (lambda (ref)
                         (list
                           (cons :path (subseq (jsown:val ref "uri") 7))
                           (cons :line (jsown:val (jsown:val (jsown:val ref "range") "start") "line"))
                           (cons :offset (jsown:val (jsown:val (jsown:val ref "range") "start") "character"))))
                       (jsown:val refs "result")))
    (setf refs (remove nil (mapcar (lambda (ref)
                                     (when (is-analysis-target (cdr (assoc :path ref)) exclude)
                                       ref))
                                   refs)))

    (start-javaparser)
    (setf refs (mapcar (lambda (ref)
                         (let ((result (exec-javaparser (cdr (assoc :path ref)))))
                           (when (> (length result) 0)
                             (acons :ast (cdr (jsown:parse result)) ref))))
                       refs))
    (stop-javaparser)

    (remove-duplicates
      (remove nil
              (mapcar (lambda (ref)
                        (let ((endpoint-pos (find-endpoint ref root-path)))
                             (if endpoint-pos
                                 endpoint-pos
                                 (progn
                                   (enqueue q (list
                                                (cons "path" (enough-namestring (cdr (assoc :path ref)) root-path))
                                                (cons "start" (cdr (assoc :line ref)))
                                                (cons "end" (cdr (assoc :line ref)))))
                                   nil))))
                      refs))
      :test #'equal)))

(defstruct queue (values nil))

(defun enqueue (q v)
  (if (null (queue-values q))
      (setf (queue-values q) (list v))
      (nconc (queue-values q) (list v))))

(defun dequeue (q)
  (unless (null (queue-values q))
    (pop (queue-values q))))

(defparameter *void-keyword* 113)
(defparameter *parenthesized-expression* 208)
(defparameter *arrow-function* 210)
(defparameter *variable-statement* 233)
(defparameter *return-statement* 243)
(defparameter *variable-declaration* 250)
(defparameter *function-declaration* 252)
(defparameter *jsx-element* 274)
(defparameter *jsx-self-closing-element* 275)
(defparameter *jsx-opening-element* 276)

(defparameter *nearest-comp-pos* nil)

(defun find-component (ast pos)
  (when (and (jsown:keyp ast "kind")
             (or
               (equal (jsown:val ast "kind") *jsx-opening-element*) 
               (equal (jsown:val ast "kind") *jsx-self-closing-element*)))
    (setq *nearest-comp-pos* (+ (jsown:val ast "start") 1)))

  (when (not (null ast))
    (if (consp (car ast))
        (progn
          (when (and (jsown:keyp ast "start") (equal (jsown:val ast "start") pos))
            (return-from find-component *nearest-comp-pos*))

        (if (consp (cdr (car ast)))
            (progn
              (let ((comp-pos (find-component (cdr (car ast)) pos)))
                (when (not (null comp-pos))
                  (return-from find-component comp-pos)))
              (return-from find-component (find-component (cdr ast) pos)))
            (progn
              (when (cdr ast)
                (return-from find-component (find-component (cdr ast) pos))))))
      (progn
        (return-from find-component (find-component (cdr ast) pos))))))

(defun find-endpoint (ref root-path)
  (let ((q (make-queue)) is-rest-controller)
    (enqueue q (cdr (assoc :ast ref)))
    (loop
      (let ((ast (dequeue q))
            (path (cdr (assoc :path ref)))
            (line (cdr (assoc :line ref))))
        (if (null ast) (return))

        (when 
          (and
            (string= (cdr (car ast)) "com.github.javaparser.ast.expr.MarkerAnnotationExpr")
            (string= (jsown:val (jsown:val ast "name") "identifier") "RestController"))
          (setf is-rest-controller t))

        (when (and
                (string= (cdr (car ast)) "com.github.javaparser.ast.body.MethodDeclaration")
                (<= (jsown:val (jsown:val ast "range") "beginLine") line)
                (> (jsown:val (jsown:val ast "range") "endLine") line)
                is-rest-controller)
          (return
            (list (cons :path (enough-namestring path root-path))
                  (cons :line (jsown:val (jsown:val ast "range") "beginLine"))
                  (cons :offset (jsown:val (jsown:val ast "range") "beginColumn")))))

        (when (jsown:keyp ast "types")
          (loop for type in (jsown:val ast "types") do
                (enqueue q (cdr type))))

        (when (jsown:keyp ast "annotations")
          (loop for annotation in (jsown:val ast "annotations") do
                (enqueue q (cdr annotation))))    

        (when (jsown:keyp ast "members")
          (loop for member in (jsown:val ast "members") do
                (enqueue q (cdr member))))))))

(defun find-child-nodes (node)
  (if (jsown:keyp node "statements")
    (jsown:val node "statements")
  (when (jsown:keyp node "parameters")
    (jsown:val node "parameters"))))

(defun inject-mark (project-path component-poss)
  (loop for pos in component-poss collect
        (let ((line-no 0))
              (with-open-file (instream (uiop:merge-pathnames* (cdr (assoc :path pos)) project-path))
                (with-open-file (outstream (uiop:merge-pathnames* (cdr (assoc :path pos)) project-path)
                                           :direction :output
                                           :if-exists :overwrite)
                  (loop for line = (read-line instream nil) while line collect
                        (progn
                          (setq line-no (+ line-no 1))
                          (if (= line-no (cdr (assoc :line pos)))
                              (format outstream "~a~%"
                                      (inject-mark-on-line line (cdr (assoc :offset pos)) 1))
                              (write-line line outstream)))))))))

(defvar *tsserver-id*)
(defun exec (command)
  (call command)
  (let ((stream (uiop:process-info-output *tsserver*)))
    (loop while stream do
          (let ((result (jsown:parse (extract-json stream))))
            (when (and
                    (jsown:keyp result "request_seq")
                    (= (jsown:val result "request_seq") *tsserver-id*))
              (return result))))))

(defun exec-tsparser (command)
  (call-tsparser command)
  (let ((stream (uiop:process-info-output *tsparser*)))
    (read-line stream)))

(defun exec-jdtls-initialize (root-path)
  (setf *jdtls-id* (+ *jdtls-id* 1))
  (exec-jdtls (format nil "{\"jsonrpc\":\"2.0\",\"id\":~a,\"method\":\"initialize\",\"params\":{\"processId\":null,\"rootPath\":\"~a\",\"rootUri\":\"file://~a\",\"capabilities\":{}}}"
                      *jdtls-id* root-path root-path))
  (let ((stream (uiop:process-info-output *jdtls*)))
    (loop while stream do
          (let ((result (jsown:parse (extract-json stream))))
            (when (and
                    (jsown:keyp result "params")
                    (jsown:keyp (jsown:val result "params") "type")
                    (string= (jsown:val (jsown:val result "params") "type") "Started"))
              (return))))))

(defvar *jdtls-id*)
(defun exec-jdtls (command)
  (call-jdtls command)
  (let ((stream (uiop:process-info-output *jdtls*)))
    (loop while stream do
          (let ((result (jsown:parse (extract-json stream))))
            (when (and
                    (jsown:keyp result "id")
                    (= (jsown:val result "id") *jdtls-id*))
              (return result))))))

(defun exec-javaparser (command)
  (call-javaparser command)
  (let ((stream (uiop:process-info-output *javaparser*)))
    (read-line stream)))

(defun call (command)
  (write-line command (uiop:process-info-input *tsserver*))
  (force-output (uiop:process-info-input *tsserver*)))

(defun call-tsparser (command)
  (write-line command (uiop:process-info-input *tsparser*))
  (force-output (uiop:process-info-input *tsparser*)))

(defun call-jdtls (command)
  (write-line (format nil "Content-Length: ~a~c~c~c~c~a"
                      (length command) #\return #\linefeed
                      #\return #\linefeed
                      command)
              (uiop:process-info-input *jdtls*))
  (force-output (uiop:process-info-input *jdtls*)))

(defun call-javaparser (command)
  (write-line command (uiop:process-info-input *javaparser*))
  (force-output (uiop:process-info-input *javaparser*)))

(defun extract-json (stream)
  ;; Content-Length: 99
  (let ((len (parse-integer (subseq (read-line stream) 16))))
    ;; newline
    (read-line stream)
    ;; JSON
    (loop
      with result = ""
      repeat len
      do (setf result (format nil "~a~a" result (read-char stream)))
      finally (return result))))

;; for debug
(defun top (sequence limit)
  (let ((line-no 0) (result ""))
    (with-input-from-string (in sequence)
      (loop :for line := (read-line in nil nil) :while line
            :do (progn
                  (setf line-no (+ line-no 1))
                  (setf result (format nil "~A~A~%" result line))
                  (when (= line-no limit)
                    (return-from top result)))))))
