all: build test lint

.PHONY: build
build:
	 @$(MAKE) -C 01_web_ui build
	 @$(MAKE) -C 03_tiles build
	 @$(MAKE) -C 04_load_map build
	 @$(MAKE) -C 05_actors build
	 @$(MAKE) -C 06_moving build
	 @$(MAKE) -C 07_collisions build
	 @$(MAKE) -C 08_layers build

.PHONY: setup
setup:
	 @$(MAKE) -C 01_web_ui setup
	 @$(MAKE) -C 03_tiles setup
	 @$(MAKE) -C 04_load_map setup
	 @$(MAKE) -C 05_actors setup
	 @$(MAKE) -C 06_moving setup
	 @$(MAKE) -C 07_collisions setup
	 @$(MAKE) -C 08_layers setup

.PHONY: test
test:
	 @$(MAKE) -C 01_web_ui test
	 @$(MAKE) -C 03_tiles test
	 @$(MAKE) -C 04_load_map test
	 @$(MAKE) -C 05_actors test
	 @$(MAKE) -C 06_moving test
	 @$(MAKE) -C 07_collisions test
	 @$(MAKE) -C 08_layers test

.PHONY: lint
lint:
	 @$(MAKE) -C 01_web_ui lint
	 @$(MAKE) -C 03_tiles lint
	 @$(MAKE) -C 04_load_map lint
	 @$(MAKE) -C 05_actors lint
	 @$(MAKE) -C 06_moving lint
	 @$(MAKE) -C 07_collisions lint
	 @$(MAKE) -C 08_layers lint


