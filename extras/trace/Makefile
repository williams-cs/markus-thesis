all: trace hello

trace: trace.cc
	g++ -std=c++17 -Wall -o trace trace.cc

hello: hello.cc
	g++ -std=c++17 -Wall -o hello hello.cc

clean:
	@rm -f trace
	@rm -f hello

.PHONY: clean