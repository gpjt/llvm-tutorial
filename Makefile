CC=g++
CFLAGS=-g -O3
LLVM_FLAGS=$(shell llvm-config --cxxflags --libs core) $(shell llvm-config --ldflags)

all: toy

toy: toy.cpp
	$(CC) $(CFLAGS) toy.cpp $(LLVM_FLAGS) -o toy

clean:
	rm -f toy
