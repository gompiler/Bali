FROM debian:stable

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN mkdir -p /repo/
WORKDIR /repo/
COPY . /repo/
RUN make test