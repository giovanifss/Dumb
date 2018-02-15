#FROM alpine:latest
#FROM ubuntu
#FROM debian
FROM samdoshi/haskell-stack
#FROM amd64/ubuntu:latest

#WORKDIR /root

#RUN apk update && \
  #       apk add libcurl curl git gcc g++ make alpine-sdk llvm3.7 perl binutils gmp-dev clang libffi-dev
  #    curl -o /dumb -L https://github.com/giovanifss/Dumb/releases/download/0.1.0/dumb && \
  #    curl -o /wordlists.tar.gz -L https://github.com/giovanifss/Dumb/releases/download/0.1.0/wordlists.tar.gz && \
  #    tar xf wordlists.tar.gz && \
  #    chmod +x /dumb

#RUN apt-get update -y && \
  #            apt-get install curl git -y
  #    curl -o /dumb -L https://github.com/giovanifss/Dumb/releases/download/0.1.0/dumb && \
  #    curl -o /wordlists.tar.gz -L https://github.com/giovanifss/Dumb/releases/download/0.1.0/wordlists.tar.gz && \
  #    tar xf wordlists.tar.gz && \
  #    chmod +x /dumb

#RUN curl -sSL https://get.haskellstack.org/ | sh && mkdir -p /Dumb
      #git clone https://github.com/giovanifss/Dumb.git && \

ADD . /Dumb

RUN cd Dumb/ && stack upgrade && stack build && stack install --local-bin-path / && \
      mv wordlists/* /

#ADD script.sh /

ENTRYPOINT ["./dumb"]
#CMD ["/script.sh"]
