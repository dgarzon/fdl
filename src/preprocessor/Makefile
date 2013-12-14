CC  = gcc
CXX = g++

INCLUDES =
CFLAGS   = -g -Wall $(INCLUDES)
CXXFLAGS = -g -Wall $(INCLUDES)

LDFLAGS =
LDLIBS =

.PHONY: default
default: preprocessor

# header dependency
preprocessor: preprocessor.o

.PHONY: clean
clean:
	rm -f *.o *.txt *~ a.out core preprocessor

.PHONY: all
all: clean default

