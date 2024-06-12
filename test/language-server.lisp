(defpackage #:inga/test/language-server
  (:use #:cl
        #:fiveam
        #:inga/language-server))
(in-package #:inga/test/language-server)

(def-suite language-server)
(in-suite language-server)

(test dequeue-msg
  (inga/language-server::init-msg-q)
  (inga/language-server::enqueue-msg '(:obj ("id" . "1") ("method" . "textDocument/didOpen")))
  (inga/language-server::enqueue-msg '(:obj ("id" . "2") ("method" . "textDocument/didChange")))
  (is (equal
        '(:obj ("id" . "1") ("method" . "textDocument/didOpen"))
        (inga/language-server::dequeue-msg))))

(test keep-last-one-when-multiple-didchange-events-are-received
  (inga/language-server::init-msg-q)
  (inga/language-server::enqueue-msg '(:obj ("id" . "1") ("method" . "textDocument/didChange")))
  (inga/language-server::enqueue-msg '(:obj ("id" . "2") ("method" . "textDocument/didChange")))
  (is (equal
        '(:obj ("id" . "2") ("method" . "textDocument/didChange"))
        (inga/language-server::dequeue-msg))))

(test discard-queue-when-shutdown-event-is-received
  (inga/language-server::init-msg-q)
  (inga/language-server::enqueue-msg '(:obj ("id" . "1") ("method" . "textDocument/didChange")))
  (inga/language-server::enqueue-msg '(:obj ("id" . "2") ("method" . "shutdown")))
  (is (equal
        '(:obj ("id" . "2") ("method" . "shutdown"))
        (inga/language-server::dequeue-msg))))

