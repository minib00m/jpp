PATH := $(PATH):$(PWD)/stack-1.1.2-linux-x86_64
SHELL := /bin/bash
export SYSTEM_CERTIFICATE_PATH=/etc/openssl/certs

all:
	wget -nc https://www.stackage.org/stack/linux-x86_64;
	tar -zxvf linux-x86_64;
	stack setup;
	stack build;
	stack install;

