FROM fukamachi/roswell:21.10.14.111-ubuntu AS build

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
  apt-get install -y curl

# https://github.com/cli/cli/blob/trunk/docs/install_linux.md#installing-gh-on-linux-and-bsd
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg && \
  chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg

RUN mkdir /tmp/inga

RUN curl -o /tmp/inga/setup_node -fsSL https://deb.nodesource.com/setup_lts.x

COPY ../.. /inga
RUN ros && \
  cd ~/.roswell/local-projects && ln -s /inga inga && \
  ros build /inga/roswell/inga.ros


FROM ubuntu:22.10

COPY --from=build /usr/share/keyrings/githubcli-archive-keyring.gpg /usr/share/keyrings/githubcli-archive-keyring.gpg
COPY --from=build /tmp/inga /tmp/inga
COPY --from=build /inga/roswell/inga /usr/local/bin/

RUN bash /tmp/inga/setup_node

RUN apt-get update && \
  apt-get install -y ca-certificates

RUN echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null && \
  apt-get update && \
  apt-get install -y \
    git \
    gh \
    nodejs && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

RUN npm install -g typescript @seachicken/tsparser

ENV INGA_HOME /
ENV INGA_CONTEXT typescript
ENV INGA_DEBUG 1

ENTRYPOINT ["inga"]

