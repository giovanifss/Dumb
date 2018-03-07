FROM bitnami/minideb:jessie

ADD dist/dumb /dumb
ADD dist/services /etc/services
ADD wordlists/* /

ENTRYPOINT ["/dumb"]
