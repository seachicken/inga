FROM fukamachi/roswell:21.10.14.111-ubuntu AS build

COPY ../.. /inga
RUN ros && \
  ln -s /inga /root/.roswell/local-projects/inga && \
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
  apt-get install -y git && \
  apt-get install -y maven && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

ENV INGA_HOME /
ENV INGA_CONTEXT java

ENTRYPOINT ["inga"]

