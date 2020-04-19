project := sokoban

build: src app
	stack build --fast

test: test build
	stack test --fast

run: build
	stack exec $(project)

release:
	stack build

.PHONY: build test run release
