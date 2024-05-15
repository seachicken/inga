FROM ubuntu:latest AS build

RUN apt-get update && \
  apt-get -y install sudo git build-essential automake libcurl4-openssl-dev && \
  git clone -b release https://github.com/roswell/roswell.git && \
  cd roswell && \
  sh bootstrap && \
  ./configure && \
  make && \
  sudo make install && \
  ros setup

ENV JAVAPARSER 0.3.4
ENV KTPARSRER 0.1.6
ENV SPRING_PROPERTY_LOADER 0.3.1
ENV JVM_DEPENDENCY_LOADER 0.5.1
ENV INGA_HOME /
ENV INGA_CONTEXT java

ADD https://github.com/seachicken/javaparser/releases/download/v$JAVAPARSER/javaparser-$JAVAPARSER.jar ./libs/javaparser.jar
ADD https://github.com/seachicken/ktparser/releases/download/v$KTPARSRER/ktparser-$KTPARSRER.jar ./libs/ktparser.jar
ADD https://github.com/seachicken/jvm-dependency-loader/releases/download/v$JVM_DEPENDENCY_LOADER/jvm-dependency-loader-$JVM_DEPENDENCY_LOADER.jar ./libs/jvm-dependency-loader.jar
ADD https://github.com/seachicken/spring-property-loader/releases/download/v$SPRING_PROPERTY_LOADER/spring-property-loader-$SPRING_PROPERTY_LOADER.jar ./libs/spring-property-loader.jar

RUN apt-get update && apt-get install -y wget apt-transport-https && \
  wget -O - https://packages.adoptium.net/artifactory/api/gpg/key/public | tee /usr/share/keyrings/adoptium.asc && \
    echo "deb [signed-by=/usr/share/keyrings/adoptium.asc] https://packages.adoptium.net/artifactory/deb $(awk -F= '/^VERSION_CODENAME/{print$2}' /etc/os-release) main" | tee /etc/apt/sources.list.d/adoptium.list && \
  apt-get update && apt-get install -y git maven build-essential automake temurin-17-jdk && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

WORKDIR /work

RUN git config --global --add safe.directory /work

COPY ../.. /root/.roswell/local-projects/inga

RUN ros install inga

ENTRYPOINT ["ros", "exec", "inga"]

