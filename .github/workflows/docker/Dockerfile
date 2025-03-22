ARG BASE_IMAGE


FROM ubuntu:jammy AS build

# https://roswell.github.io/Installation.html
RUN apt-get update && \
    apt-get install -y curl sudo bzip2 make jq libcurl3-openssl-dev && \
    curl -L https://github.com/roswell/roswell/releases/download/v23.10.14.114/roswell_23.10.14.114-1_amd64.deb --output roswell.deb && \
    sudo dpkg -i roswell.deb

COPY ../../../.. /inga

RUN ros && ros install fiveam


FROM $BASE_IMAGE

ENV LISP=sbcl-bin
ENV INGA_HOME=/inga

COPY --from=build /usr/bin/ros /usr/local/bin/ros
COPY --from=build /root/.roswell /root/.roswell
COPY --from=build /etc/roswell /etc/roswell
COPY --from=build /usr/lib/x86_64-linux-gnu/libcurl.so.4 /usr/lib/
COPY --from=build /inga /inga

WORKDIR /inga

SHELL ["/busybox/sh", "-c"]

RUN ln -s /inga /root/.roswell/local-projects/inga

# https://github.com/cli/cli/issues/9569
#RUN rm -f /etc/apt/sources.list.d/github-cli.list

ENTRYPOINT ["sh"]

