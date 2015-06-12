CC=g++
CFLAGS=-g -O3 -rdynamic
LLVM_FLAGS=$(shell llvm-config --cxxflags --libs core jit native) $(shell llvm-config --ldflags)

all: toy

toy: toy.cpp
	$(CC) $(CFLAGS) toy.cpp $(LLVM_FLAGS) -o toy

clean:
	rm -f toy
