all: build test lint

.PHONY: build
build:
	 @$(MAKE) -C 01_web_ui build

.PHONY: setup
setup:
	 @$(MAKE) -C 01_web_ui setup

.PHONY: test
test:
	 @$(MAKE) -C 01_web_ui test

.PHONY: lint
lint:
	 @$(MAKE) -C 01_web_ui lint


