all: main

main: src/*.cc
	g++ -std=c++17 -Ilib/boost -Llib/boost/stage/lib -Wall -o main src/*.cc -lboost_program_options -lboost_filesystem

clean:
	@rm -f main

.PHONY: clean