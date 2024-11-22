(defpackage #:inga/test/server
  (:use #:cl
        #:fiveam
        #:inga/server))
(in-package #:inga/test/server)

(def-suite server)
(in-suite server)

(test discard-messages-before-initialize
  (inga/server::init-msg-q)
  (inga/server::enqueue-msg '(:obj ("id" . "1") ("method" . "textDocument/didChange")))
  (inga/server::enqueue-msg '(:obj ("id" . "2") ("method" . "initialize")))
  (is (equal
        '(:obj ("id" . "2") ("method" . "initialize"))
        (inga/server::dequeue-msg))))

(test dequeue-message
  (inga/server::init-msg-q)
  (inga/server::enqueue-msg '(:obj ("id" . "1") ("method" . "initialize")))
  (inga/server::enqueue-msg '(:obj ("id" . "2") ("method" . "textDocument/didChange")))
  (is (equal
        '(:obj ("id" . "1") ("method" . "initialize"))
        (inga/server::dequeue-msg))))

(test keep-last-one-when-multiple-didchange-messagess-are-received
  (inga/server::init-msg-q)
  (inga/server::enqueue-msg '(:obj ("id" . "1") ("method" . "initialize")))
  (inga/server::enqueue-msg '(:obj ("id" . "2") ("method" . "textDocument/didChange")))
  (inga/server::enqueue-msg '(:obj ("id" . "3") ("method" . "textDocument/didChange")))
  (inga/server::dequeue-msg)
  (is (equal
        '(:obj ("id" . "3") ("method" . "textDocument/didChange"))
        (inga/server::dequeue-msg))))

(test discard-messages-when-shutdown-message-is-received
  (inga/server::init-msg-q)
  (inga/server::enqueue-msg '(:obj ("id" . "1") ("method" . "initialize")))
  (inga/server::enqueue-msg '(:obj ("id" . "2") ("method" . "textDocument/didChange")))
  (inga/server::enqueue-msg '(:obj ("id" . "3") ("method" . "shutdown")))
  (is (equal
        '(:obj ("id" . "3") ("method" . "shutdown"))
        (inga/server::dequeue-msg))))

(test get-relative-path
  (is (equal
        "src/main/java/inga/App.java"
        (inga/server::get-relative-path
          "/work/service-b/src/main/java/inga/App.java"
          '("/work/service-a/"
            "/work/service-b/")))))

