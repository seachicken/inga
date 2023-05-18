FROM fukamachi/roswell:21.10.14.111-ubuntu AS build

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
  apt-get install -y curl

# https://github.com/cli/cli/blob/trunk/docs/install_linux.md#installing-gh-on-linux-and-bsd
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg && \
  chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg

RUN mkdir libs

RUN curl -o ./libs/javaparser.jar -L https://github.com/seachicken/javaparser/releases/download/javaparser-0.1.4/javaparser-0.1.4.jar

RUN curl -o ./libs/ktparser.jar -L https://github.com/seachicken/ktparser/releases/download/ktparser-0.1.3/ktparser-0.1.3.jar

COPY ../.. /inga
RUN ros && \
  cd ~/.roswell/local-projects && ln -s /inga inga && \
  ros build /inga/roswell/inga.ros


FROM ubuntu:latest

COPY --from=build /usr/share/keyrings/githubcli-archive-keyring.gpg /usr/share/keyrings/githubcli-archive-keyring.gpg
COPY --from=build /libs /libs
COPY --from=build /inga/roswell/inga /usr/local/bin/

RUN apt-get update && \
  apt-get install -y ca-certificates

RUN echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null && \
  apt-get update && \
  apt-get install -y \
    git \
    gh \
    openjdk-17-jdk && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

ENV INGA_HOME /
ENV INGA_CONTEXT java
ENV INGA_DEBUG 1

ENTRYPOINT ["inga"]

