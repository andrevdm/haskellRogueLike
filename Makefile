all: build test lint

.PHONY: build
build:
	 @$(MAKE) -C 01_web_ui build
	 @$(MAKE) -C 03_tiles build

.PHONY: setup
setup:
	 @$(MAKE) -C 01_web_ui setup
	 @$(MAKE) -C 03_tiles setup

.PHONY: test
test:
	 @$(MAKE) -C 01_web_ui test
	 @$(MAKE) -C 03_tiles test

.PHONY: lint
lint:
	 @$(MAKE) -C 01_web_ui lint
	 @$(MAKE) -C 03_tiles lint


