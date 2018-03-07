USER := $(shell whoami)
GROUP := $(shell groups ${USER} | awk -F ' ' '{print $NF}')
PWD := $(shell pwd)
IMAGE="giovanifss/dumb:beta"

fix-permissions:
	sudo chown -R ${USER}:${GROUP} .stack-work dist

remove-old:
	-rm dist/dumb

build-dumb: remove-old
	docker run -v ${PWD}:/opt -it giovanifss/minideb-stack install --allow-different-user --local-bin-path dist/

build: build-dumb fix-permissions
	docker build -t ${IMAGE} .

push:
	docker push ${IMAGE}
