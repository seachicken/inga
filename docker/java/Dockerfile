ARG INGA_TAG=latest
ARG JAVA_VERSION=21

FROM eclipse-temurin:$JAVA_VERSION-jdk-jammy AS build-cri

ARG JAVA_VERSION

WORKDIR /work

RUN if [ "$JAVA_VERSION" = "8" ]; then \
      which java; \
      cp -r /opt/java/openjdk java-inga; \
    else \
      jlink --add-modules java.base,jdk.compiler --output java-inga; \
    fi


FROM ghcr.io/seachicken/inga:$INGA_TAG

ARG JAVA_VERSION

ENV JAVAPARSER=0.7.3
ENV KTPARSRER=0.1.7
ENV SPRING_PROPERTY_LOADER=0.3.2
ENV JVM_DEPENDENCY_LOADER=0.9.0
ENV JAVA_HOME=/usr/local/java-inga
ENV PATH=$JAVA_HOME/bin:$PATH

COPY --from=build-cri /work/java-inga /usr/local/java-inga

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

