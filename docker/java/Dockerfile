FROM fukamachi/roswell:21.10.14.111-ubuntu AS build

COPY ../.. /inga
RUN ros && \
  cd ~/.roswell/local-projects && ln -s /inga inga && \
  ros build /inga/roswell/inga.ros


FROM eclipse-temurin:17-jdk-jammy

ENV JAVAPARSER 0.1.7
ENV KTPARSRER 0.1.3
ENV SPRING_PROPERTY_LOADER 0.1.0
ENV JVM_DEPENDENCY_LOADER 0.1.1

COPY --from=build /inga/roswell/inga /usr/local/bin/

ADD https://github.com/seachicken/javaparser/releases/download/javaparser-$JAVAPARSER/javaparser-$JAVAPARSER.jar ./libs/javaparser.jar
ADD https://github.com/seachicken/ktparser/releases/download/ktparser-$KTPARSRER/ktparser-$KTPARSRER.jar ./libs/ktparser.jar 
ADD https://github.com/seachicken/spring-property-loader/releases/download/spring-property-loader-$SPRING_PROPERTY_LOADER/spring-property-loader-$SPRING_PROPERTY_LOADER.jar ./libs/spring-property-loader.jar
ADD https://github.com/seachicken/jvm-dependency-loader/releases/download/jvm-dependency-loader-$JVM_DEPENDENCY_LOADER/jvm-dependency-loader-$JVM_DEPENDENCY_LOADER.jar ./libs/jvm-dependency-loader.jar

RUN apt-get update && \
  apt-get install -y git && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

ENV INGA_HOME /
ENV INGA_CONTEXT java

ENTRYPOINT ["inga"]

