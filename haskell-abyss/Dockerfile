FROM haskell:8.10-buster as build
RUN stack upgrade && stack setup
COPY . /opt/build
WORKDIR /opt/build
RUN stack install
RUN stack build --system-ghc
RUN mkdir -p /opt/app && stack --local-bin-path=/opt/app install

FROM debian:buster-slim
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apt-get update
COPY --from=build /opt/app/* .