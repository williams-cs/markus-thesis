all: build

build:
	dune build
	ln -s -f ./_build/default/bin/shard_runner.exe ./shard.exe

clean:
	@rm -f shard.exe
	@rm -rf _build

.PHONY: build clean