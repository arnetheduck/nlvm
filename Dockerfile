FROM ubuntu:latest AS build

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
FROM ubuntu:latest

COPY --from=build /usr/bin/nlvm /usr/bin/nlvm
COPY --from=build /usr/lib/nlvm /usr/lib/nlvm
COPY --from=build /usr/lib/nim /usr/lib/nim
COPY --from=build /usr/lib/clang /usr/lib/clang

RUN apt-get update && apt-get install -y libc-dev libstdc++-dev && rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["/usr/bin/nlvm"]
