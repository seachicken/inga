ARG INGA_IMAGE=ghcr.io/seachicken/inga


FROM $INGA_IMAGE

ENV TSPARSER=0.1.1

RUN apt-get update && \
    apt-get install -y curl && \
    curl -fsSL https://deb.nodesource.com/setup_18.x | bash - && \
    apt-get install -y nodejs && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    npm install -g typescript @seachicken/tsparser@$TSPARSER

WORKDIR /work

ENV INGA_HOME=/
ENV INGA_CONTEXT=typescript

ENTRYPOINT ["inga"]

