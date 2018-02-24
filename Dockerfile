#FROM bitnami/minideb:jessie
FROM debian:jessie

ADD dist/dumb /dumb
ADD wordlists/* /

ENTRYPOINT ["/dumb"]
