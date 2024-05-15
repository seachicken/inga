FROM ubuntu:latest AS build

ENV CI true

RUN apt-get update && \
  apt-get install -y curl sudo git build-essential automake libcurl4-openssl-dev && \
  curl -L https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh | sh && \
  ls -lsa /usr/local && \
  run ros install sbcl-bin/2.3.11 && ros use sbcl-bin/2.3.11

COPY ../.. /inga

RUN ln -s /inga /root/.roswell/local-projects/inga && \
  ros build /inga/roswell/inga.ros


FROM eclipse-temurin:21-jdk-jammy

ENV JAVAPARSER 0.3.4
ENV KTPARSRER 0.1.6
ENV SPRING_PROPERTY_LOADER 0.3.1
ENV JVM_DEPENDENCY_LOADER 0.5.1

COPY --from=build /inga/roswell/inga /usr/local/bin/

ADD https://github.com/seachicken/javaparser/releases/download/v$JAVAPARSER/javaparser-$JAVAPARSER.jar ./libs/javaparser.jar
ADD https://github.com/seachicken/ktparser/releases/download/v$KTPARSRER/ktparser-$KTPARSRER.jar ./libs/ktparser.jar
ADD https://github.com/seachicken/jvm-dependency-loader/releases/download/v$JVM_DEPENDENCY_LOADER/jvm-dependency-loader-$JVM_DEPENDENCY_LOADER.jar ./libs/jvm-dependency-loader.jar
ADD https://github.com/seachicken/spring-property-loader/releases/download/v$SPRING_PROPERTY_LOADER/spring-property-loader-$SPRING_PROPERTY_LOADER.jar ./libs/spring-property-loader.jar

RUN apt-get update && \
  apt-get install -y git maven && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

WORKDIR /work

RUN git config --global --add safe.directory /work

ENV INGA_HOME /
ENV INGA_CONTEXT java

ENTRYPOINT ["inga"]

