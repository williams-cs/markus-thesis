all: main

main: src/*.cc
	g++ -std=c++17 -I lib/boost -Wall -o main src/*.cc

clean:
	@rm -f main

.PHONY: clean