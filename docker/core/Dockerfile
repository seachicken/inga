FROM fukamachi/roswell:23.10.14.114-debian AS build

RUN apt-get update && \
    apt-get install -y build-essential

COPY ../.. /inga

RUN ros && \
    ln -s /inga /root/.roswell/local-projects/inga && \
    ros build /inga/roswell/inga.ros


FROM gcr.io/distroless/base-debian12

COPY --from=build /inga/roswell/inga /usr/local/bin/

