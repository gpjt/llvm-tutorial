CC=gcc
CFLAGS=-g -O3
LLVM_FLAGS=`llvm-config --cxxflags --ldflags --libs core`

all: toy

toy: toy.cpp
	$(CC) $(CFLAGS) toy.cpp $(LLVM_FLAGSi) -o toy

clean:
	rm -f toy
