FROM debian:stable

ENV PATH ${PATH}:/root/.local/bin

RUN set -ex \
        && apt-get update\
        && apt-get install -y make openjdk-8-jdk python3 curl\
        && curl -sSL https://get.haskellstack.org/ | sh

RUN mkdir -p /Bali/
WORKDIR /Bali/
COPY . /Bali/
CMD ["./docker_test.sh"]
