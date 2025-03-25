ARG INGA_IMAGE=ghcr.io/seachicken/inga
ARG JAVA_VERSION=21


FROM eclipse-temurin:$JAVA_VERSION-jdk-noble AS build-parser

WORKDIR /work

RUN cp -r /opt/java/openjdk inga-jdk


FROM $INGA_IMAGE

ARG JAVA_VERSION

ENV JAVAPARSER=0.7.4
ENV KTPARSRER=0.1.7
ENV SPRING_PROPERTY_LOADER=0.3.2
ENV JVM_DEPENDENCY_LOADER=0.9.1

COPY --from=build-parser /work/inga-jdk /usr/local/inga-jdk

ADD https://github.com/seachicken/javaparser/releases/download/v$JAVAPARSER/javaparser-$JAVA_VERSION-$JAVAPARSER.jar ./libs/javaparser.jar
ADD https://github.com/seachicken/ktparser/releases/download/v$KTPARSRER/ktparser-$KTPARSRER.jar ./libs/ktparser.jar
ADD https://github.com/seachicken/jvm-dependency-loader/releases/download/v$JVM_DEPENDENCY_LOADER/jvm-dependency-loader-$JVM_DEPENDENCY_LOADER.jar ./libs/jvm-dependency-loader.jar
ADD https://github.com/seachicken/spring-property-loader/releases/download/v$SPRING_PROPERTY_LOADER/spring-property-loader-$SPRING_PROPERTY_LOADER.jar ./libs/spring-property-loader.jar

RUN apt-get update && \
    apt-get install -y maven && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /work

ENV INGA_HOME=/
ENV INGA_CONTEXT=java

ENTRYPOINT ["inga"]

