all: build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build 
	diff ../07_collisions/src/ ./src/ -w -B -a -d -u -b > changes.patch | true

.PHONY: test
test:
	stack test


.PHONY: lint
lint:
	hlint .
#	weeder .

