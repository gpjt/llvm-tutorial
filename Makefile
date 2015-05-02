CC=g++
CFLAGS=-g -O3
LLVM_FLAGS=$(shell llvm-config --cxxflags --ldflags --libs core)

all: toy

toy: toy.cpp
	$(CC) $(CFLAGS) toy.cpp $(LLVM_FLAGS) -o toy

clean:
	rm -f toy
