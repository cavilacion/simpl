FROM debian:bullseye
LABEL maintainer="erikvoogd@ifi.uio.no"
LABEL build_date="2022-06-30"

WORKDIR /code
RUN apt-get update 
RUN apt-get install -y git racket sqlite3
RUN raco pkg install --auto beautiful-racket
RUN git clone https://github.com/cavilacion/simpl
WORKDIR /code/simpl
RUN raco pkg install
