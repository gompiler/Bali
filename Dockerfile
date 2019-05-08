FROM debian:stable

RUN set -ex \
        && apt-get update\
        && apt-get install -y make openjdk-8-jre python3 curl\
        && curl -sSL https://get.haskellstack.org/ | sh

ENV PATH ${PATH}:/root/.local/bin

RUN mkdir -p /repo/
WORKDIR /repo/
COPY . /repo/
CMD ["make", "gen"]
CMD ["make", "test"]
