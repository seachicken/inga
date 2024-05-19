FROM fukamachi/sbcl:2.4.4 AS build

COPY ../.. /root/common-lisp
WORKDIR /root/common-lisp

RUN set -x; \
  sbcl --load inga.asd \
  --eval "(ql:quickload :inga)"

