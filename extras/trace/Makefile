all: trace hello

trace: trace.cpp
	g++ -std=c++17 -Wall -o trace trace.cpp

hello: hello.cpp
	g++ -std=c++17 -Wall -o hello hello.cpp

clean:
	@rm -f trace
	@rm -f hello

.PHONY: clean