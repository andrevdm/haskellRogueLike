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
	 @$(MAKE) -C 09_viewport_scroll build
	 @$(MAKE) -C 10_fov build
	 @$(MAKE) -C 11_sticky_light build
	 @$(MAKE) -C 12_energy build
	 @$(MAKE) -C 13_utility build
	 @$(MAKE) -C 14_utility_annotate build
	 @$(MAKE) -C 15_memory build
	 @$(MAKE) -C 16_debug build
	 @$(MAKE) -C 17_levels build

.PHONY: setup
setup:
	 @$(MAKE) -C 01_web_ui setup
	 @$(MAKE) -C 03_tiles setup
	 @$(MAKE) -C 04_load_map setup
	 @$(MAKE) -C 05_actors setup
	 @$(MAKE) -C 06_moving setup
	 @$(MAKE) -C 07_collisions setup
	 @$(MAKE) -C 08_layers setup
	 @$(MAKE) -C 09_viewport_scroll setup
	 @$(MAKE) -C 10_fov setup
	 @$(MAKE) -C 11_sticky_light setup
	 @$(MAKE) -C 12_energy setup
	 @$(MAKE) -C 13_utility setup
	 @$(MAKE) -C 14_utility_annotate setup
	 @$(MAKE) -C 15_memory setup
	 @$(MAKE) -C 16_debug setup
	 @$(MAKE) -C 17_levels setup

.PHONY: test
test:
	 @$(MAKE) -C 01_web_ui test
	 @$(MAKE) -C 03_tiles test
	 @$(MAKE) -C 04_load_map test
	 @$(MAKE) -C 05_actors test
	 @$(MAKE) -C 06_moving test
	 @$(MAKE) -C 07_collisions test
	 @$(MAKE) -C 08_layers test
	 @$(MAKE) -C 09_viewport_scroll test
	 @$(MAKE) -C 10_fov test
	 @$(MAKE) -C 11_sticky_light test
	 @$(MAKE) -C 12_energy test
	 @$(MAKE) -C 13_utility test
	 @$(MAKE) -C 14_utility_annotate test
	 @$(MAKE) -C 15_memory test
	 @$(MAKE) -C 16_debug test
	 @$(MAKE) -C 17_levels test

.PHONY: lint
lint:
	 @$(MAKE) -C 01_web_ui lint
	 @$(MAKE) -C 03_tiles lint
	 @$(MAKE) -C 04_load_map lint
	 @$(MAKE) -C 05_actors lint
	 @$(MAKE) -C 06_moving lint
	 @$(MAKE) -C 07_collisions lint
	 @$(MAKE) -C 08_layers lint
	 @$(MAKE) -C 09_viewport_scroll lint
	 @$(MAKE) -C 10_fov lint
	 @$(MAKE) -C 11_sticky_light lint
	 @$(MAKE) -C 12_energy lint
	 @$(MAKE) -C 13_utility lint
	 @$(MAKE) -C 14_utility_annotate lint
	 @$(MAKE) -C 15_memory lint
	 @$(MAKE) -C 16_debug lint
	 @$(MAKE) -C 17_levels lint


