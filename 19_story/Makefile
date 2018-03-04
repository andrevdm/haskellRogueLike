all: build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build 
	diff ../18_multi_level/src/ ./src/ -w -B -a -d -u -b -r --new-file > changes.patch | true
	diff ../18_multi_level/app/ ./app/ -w -B -a -d -u -b -r --new-file >> changes.patch | true
	diff ../18_multi_level/html/rogue.html ./html/rogue.html -w -B -a -d -u -b >> changes.patch | true
	diff ../18_multi_level/html/rogue.js ./html/rogue.js -w -B -a -d -u -b >> changes.patch | true
	diff ../18_multi_level/test ./test -w -B -a -d -u -b --new-file >> changes.patch | true

.PHONY: test
test:
	stack test


.PHONY: lint
lint:
	hlint .
#	weeder .

