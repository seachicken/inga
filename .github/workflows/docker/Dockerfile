ARG BASE_IMAGE


FROM ubuntu:jammy AS build

# https://roswell.github.io/Installation.html
RUN apt-get update && \
    apt-get install -y curl sudo bzip2 make jq libcurl3-openssl-dev && \
    curl -L https://github.com/roswell/roswell/releases/download/v23.10.14.114/roswell_23.10.14.114-1_amd64.deb --output roswell.deb && \
    sudo dpkg -i roswell.deb

RUN ros && ros install fiveam

COPY ../../../.. /root/.roswell/local-projects/inga


FROM $BASE_IMAGE

ENV LISP=sbcl-bin
ENV INGA_HOME=/root/.roswell/local-projects/inga

COPY --from=build /usr/bin/ros /usr/local/bin/ros
COPY --from=build /root/.roswell /root/.roswell
COPY --from=build /etc/roswell /etc/roswell
COPY --from=build /usr/lib/x86_64-linux-gnu/libcurl.so.4 /usr/lib/

#WORKDIR /inga

# https://github.com/cli/cli/issues/9569
#RUN rm -f /etc/apt/sources.list.d/github-cli.list

RUN ls -lsa /root/.roswell/local-projects/inga/test/fixtures/react-typescript-todo

ENTRYPOINT ["bash"]

