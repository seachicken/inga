(defpackage #:inga/test/server
  (:use #:cl
        #:fiveam
        #:inga/server))
(in-package #:inga/test/server)

(def-suite server)
(in-suite server)

(test dequeue-msg
  (inga/server::init-msg-q)
  (inga/server::enqueue-msg '(:obj ("id" . "1") ("method" . "textDocument/didOpen")))
  (inga/server::enqueue-msg '(:obj ("id" . "2") ("method" . "textDocument/didChange")))
  (is (equal
        '(:obj ("id" . "1") ("method" . "textDocument/didOpen"))
        (inga/server::dequeue-msg))))

(test keep-last-one-when-multiple-didchange-events-are-received
  (inga/server::init-msg-q)
  (inga/server::enqueue-msg '(:obj ("id" . "1") ("method" . "textDocument/didChange")))
  (inga/server::enqueue-msg '(:obj ("id" . "2") ("method" . "textDocument/didChange")))
  (is (equal
        '(:obj ("id" . "2") ("method" . "textDocument/didChange"))
        (inga/server::dequeue-msg))))

(test discard-queue-when-shutdown-event-is-received
  (inga/server::init-msg-q)
  (inga/server::enqueue-msg '(:obj ("id" . "1") ("method" . "textDocument/didChange")))
  (inga/server::enqueue-msg '(:obj ("id" . "2") ("method" . "shutdown")))
  (is (equal
        '(:obj ("id" . "2") ("method" . "shutdown"))
        (inga/server::dequeue-msg))))

(test get-relative-path
  (is (equal
        "src/main/java/inga/App.java"
        (inga/server::get-relative-path
          "/work/service-b/src/main/java/inga/App.java"
          '("/work/service-a/"
            "/work/service-b/")))))

