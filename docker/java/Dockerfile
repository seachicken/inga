FROM fukamachi/roswell:21.10.14.111-ubuntu AS build

COPY ../.. /inga
RUN ros && \
  cd ~/.roswell/local-projects && ln -s /inga inga && \
  ros build /inga/roswell/inga.ros


FROM eclipse-temurin:17-jre-jammy

ENV JAVAPARSER 0.1.6
ENV KTPARSRER 0.1.3

COPY --from=build /inga/roswell/inga /usr/local/bin/

ADD https://github.com/seachicken/javaparser/releases/download/javaparser-$JAVAPARSER/javaparser-$JAVAPARSER.jar ./libs/javaparser.jar
ADD https://github.com/seachicken/ktparser/releases/download/ktparser-$KTPARSRER/ktparser-$KTPARSRER.jar ./libs/ktparser.jar 

RUN apt-get update && \
  apt-get install -y git && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

ENV INGA_HOME /
ENV INGA_CONTEXT java

ENTRYPOINT ["inga"]

