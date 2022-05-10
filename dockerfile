FROM debian:bullseye
WORKDIR /code
RUN apt-get update 
RUN apt-get install -y git racket sqlite3
RUN raco pkg install --auto beautiful-racket
RUN git clone https://github.com/cavilacion/simpl
WORKDIR /code/simpl
RUN raco pkg install
