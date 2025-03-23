FROM fukamachi/roswell:23.10.14.114-ubuntu AS build

RUN apt-get update && \
    apt-get install -y build-essential

COPY ../.. /inga

RUN ros && \
    ln -s /inga /root/.roswell/local-projects/inga && \
    ros build /inga/roswell/inga.ros


FROM ubuntu:noble

COPY --from=build /inga/roswell/inga /usr/local/bin/

ENV INGA_CONTAINER=1

