FROM debian:stable

RUN set -ex \
        && apt-get update\
        && apt-get install -y make curl\
        && curl -sSL https://get.haskellstack.org/ | sh

RUN mkdir -p /repo/
WORKDIR /repo/
COPY . /repo/
RUN make test