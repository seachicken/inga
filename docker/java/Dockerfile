FROM fukamachi/roswell:21.10.14.111-ubuntu AS build

ARG VERSION
ARG DEBIAN_FRONTEND=noninteractive

RUN set -eux && \
  apt-get update && \
  apt-get install -y curl

# https://github.com/cli/cli/blob/trunk/docs/install_linux.md#installing-gh-on-linux-and-bsd
RUN set -eux && \
  curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg && \
  chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg

RUN mkdir libs

RUN set -eux && \
  curl -o ./jdtls.tar.gz -L https://download.eclipse.org/jdtls/milestones/1.15.0/jdt-language-server-1.15.0-202208311644.tar.gz && \
  mkdir libs/jdtls && \
  tar xvf ./jdtls.tar.gz -C ./libs/jdtls

RUN curl -o ./libs/javaparser.jar -L https://github.com/seachicken/javaparser/releases/download/javaparser-0.1.0/javaparser-0.1.0.jar

RUN curl -o ./libs/lombok.jar -L https://projectlombok.org/downloads/lombok.jar

RUN set -eux && \
  curl -o ./inga.tar.gz -L https://github.com/seachicken/inga/archive/refs/tags/$VERSION.tar.gz && \
  mkdir inga && \
  tar xvf ./inga.tar.gz -C ./inga --strip-components 1 && \
  ros && \
  cd ~/.roswell/local-projects && ln -s /inga inga && \
  ros build /inga/roswell/inga.ros


FROM golang:1.18.6 AS go-build
WORKDIR /build

RUN set -eux && \
  apt-get update && \
  apt-get install -y unzip

# https://github.com/cli/cli/pull/6384
RUN set -eux && \
  curl -o cli.zip -L https://github.com/seachicken/cli/archive/refs/heads/issue3613.zip && \
  unzip cli.zip && \
  cd cli-issue3613 && \
  make


FROM ubuntu:22.04

COPY --from=build /usr/share/keyrings/githubcli-archive-keyring.gpg /usr/share/keyrings/githubcli-archive-keyring.gpg
COPY --from=build /libs /libs
COPY --from=build /inga/roswell/inga /usr/local/bin/
COPY --from=go-build /build/cli-issue3613/bin/gh /usr/local/bin/

RUN set -eux && \
  apt-get update && \
  apt-get install -y git

RUN set -eux && \
  echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null && \
  apt update && \
  #apt install -y gh && \
  apt install -y openjdk-17-jdk

ENV INGA_HOME /
ENV INGA_CONTEXT java
ENV INGA_DEBUG 1

ENTRYPOINT ["inga"]

