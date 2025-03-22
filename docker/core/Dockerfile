ARG BASE_IMAGE=gcr.io/distroless/static-debian12


FROM fukamachi/roswell:23.10.14.114-debian AS build

RUN apt-get update && \
    apt-get install -y build-essential

COPY ../.. /inga

RUN ros && \
    ln -s /inga /root/.roswell/local-projects/inga && \
    ros build /inga/roswell/inga.ros


FROM $BASE_IMAGE

COPY --from=build /inga/roswell/inga /usr/local/bin/

