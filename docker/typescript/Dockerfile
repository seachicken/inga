FROM fukamachi/roswell:23.10.14.114-alpine AS build

COPY ../.. /inga
RUN ros && \
  ln -s /inga /root/.roswell/local-projects/inga && \
  ros build /inga/roswell/inga.ros


FROM node:18-slim

ENV TSPARSER=0.1.1

COPY --from=build /inga/roswell/inga /usr/local/bin/

RUN npm install -g typescript @seachicken/tsparser@$TSPARSER

WORKDIR /work

ENV INGA_HOME=/
ENV INGA_CONTEXT=typescript

ENTRYPOINT ["inga"]

