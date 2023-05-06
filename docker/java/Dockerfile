FROM fukamachi/roswell:22.12.14.113-alpine AS build

RUN apk update && apk upgrade && \
  apk add curl

RUN mkdir libs

RUN curl -o ./libs/javaparser.jar -L https://github.com/seachicken/javaparser/releases/download/javaparser-0.1.3/javaparser-0.1.3.jar

RUN curl -o ./libs/ktparser.jar -L https://github.com/seachicken/ktparser/releases/download/ktparser-0.1.2/ktparser-0.1.2.jar

COPY ../.. /inga
RUN ros && \
  cd ~/.roswell/local-projects && ln -s /inga inga && \
  ros build /inga/roswell/inga.ros


FROM alpine:3.17.3

COPY --from=build /libs /libs
COPY --from=build /inga/roswell/inga /usr/local/bin/

RUN apk update && \
    apk --no-cache add \
    git \
    github-cli \
    openjdk17-jre-headless

ENV INGA_HOME /
ENV INGA_CONTEXT java
ENV INGA_DEBUG 1

ENTRYPOINT ["inga"]

