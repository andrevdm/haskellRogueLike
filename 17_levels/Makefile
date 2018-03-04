all: build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build 
	diff ../16_debug/src/ ./src/ -w -B -a -d -u -b -r --new-file > changes.patch | true
	diff ../16_debug/app/ ./app/ -w -B -a -d -u -b -r --new-file >> changes.patch | true
	diff ../16_debug/html/rogue.html ./html/rogue.html -w -B -a -d -u -b >> changes.patch | true
	diff ../16_debug/html/rogue.js ./html/rogue.js -w -B -a -d -u -b >> changes.patch | true
	diff ../16_debug/test ./test -w -B -a -d -u -b --new-file >> changes.patch | true

.PHONY: test
test:
	stack test


.PHONY: lint
lint:
	hlint .
#	weeder .

