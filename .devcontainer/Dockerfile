FROM haskell:8.10-buster

RUN apt-get update && apt-get -y install --no-install-recommends git

RUN stack install hlint stylish-haskell

CMD rm /etc/localtime && ln -s /usr/share/zoneinfo/Asia/Tokyo /etc/localtime

COPY ./pre-commit ./.git/hooks/pre-commit