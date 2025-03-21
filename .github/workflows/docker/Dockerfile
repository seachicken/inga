ARG BASE_IMAGE


FROM ubuntu:jammy AS build

# https://roswell.github.io/Installation.html
RUN apt-get update && \
    apt-get install -y curl sudo bzip2 make jq && \
    curl -L https://github.com/roswell/roswell/releases/download/v23.10.14.114/roswell_23.10.14.114-1_amd64.deb --output roswell.deb && \
    sudo dpkg -i roswell.deb

RUN which ros


FROM $BASE_IMAGE

ENV LISP=sbcl-bin
ENV INGA_HOME=/inga

COPY --from=build /usr/bin/ros /usr/local/bin/ros

WORKDIR /inga

# https://github.com/cli/cli/issues/9569
#RUN rm -f /etc/apt/sources.list.d/github-cli.list

RUN ros install fiveam

COPY ../../../.. .

RUN if [ -d /libs ]; then mv /libs .; fi

RUN ros && \
    ln -s /inga /root/.roswell/local-projects/inga

ENTRYPOINT ["bash"]

