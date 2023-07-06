FROM fukamachi/roswell:latest-debian AS build

COPY ../.. /inga
RUN ros && \
  cd ~/.roswell/local-projects && ln -s /inga inga && \
  ros build /inga/roswell/inga.ros


FROM node:20-slim

COPY --from=build /inga/roswell/inga /usr/local/bin/

RUN apt-get update && \
  apt-get install -y git && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

RUN npm install -g typescript @seachicken/tsparser

ENV INGA_HOME /
ENV INGA_CONTEXT typescript

ENTRYPOINT ["inga"]

