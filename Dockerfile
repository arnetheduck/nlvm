FROM ubuntu:18.04 AS build

ADD . /code
WORKDIR /code

RUN apt-get update && \
    apt-get install -y rsync time git apt-utils libpcre3-dev libtinfo-dev libz-dev libssl-dev libsqlite3-dev build-essential cmake ninja-build python-minimal wget && \
    rm -rf /var/lib/apt/lists/* && \
    git submodule update --init && \
    ./make-dist-docker.sh && \
    cd / && \
    rm -rf /code/ && \
    apt-get remove -y git apt-utils libpcre3-dev libssl-dev libsqlite3-dev build-essential cmake ninja-build python-minimal wget

# Create final image
FROM ubuntu:18.04

COPY --from=build /usr/bin/nlvm /usr/bin/nlvm
COPY --from=build /usr/lib/ /usr/lib/
RUN apt-get update && apt-get install -y gcc && rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["/usr/bin/nlvm"]

