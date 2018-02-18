all: build test lint

.PHONY: build
build:
	 @$(MAKE) -C 01_web_ui build
	 @$(MAKE) -C 03_tiles build
	 @$(MAKE) -C 04_load_map build
	 @$(MAKE) -C 05_actors build

.PHONY: setup
setup:
	 @$(MAKE) -C 01_web_ui setup
	 @$(MAKE) -C 03_tiles setup
	 @$(MAKE) -C 04_load_map setup
	 @$(MAKE) -C 05_actors setup

.PHONY: test
test:
	 @$(MAKE) -C 01_web_ui test
	 @$(MAKE) -C 03_tiles test
	 @$(MAKE) -C 04_load_map test
	 @$(MAKE) -C 05_actors test

.PHONY: lint
lint:
	 @$(MAKE) -C 01_web_ui lint
	 @$(MAKE) -C 03_tiles lint
	 @$(MAKE) -C 04_load_map lint
	 @$(MAKE) -C 05_actors lint


