all: build

build:
	dune build
	ln -s -f ./_build/default/bin/dresh.exe ./dresh.exe

clean:
	@rm -f dresh.exe
	@rm -rf _build

.PHONY: build clean