C  = gcc
CXX = g++

INCLUDES =

CFLAGS   = -g -Wall $(INCLUDES)
CXXFLAGS = -g -Wall $(INCLUDES)

LDFLAGS = -g

LDLIBS =

# mylist-test: mylist-test.o libmylist.a

# mylist-test.o: mylist-test.c mylist.h

default: liblist.a libpath.a

liblist.a: list.o
	ar rc liblist.a list.o
	ranlib liblist.a

libpath.a: path.o
	ar rc libpath.a path.o
	ranlib libpath.a

list.o: list.c list.h

path.o: path.c path.h

.PHONY: clean
clean:
	rm -f *.o *.txt a.out core liblist.a libpath.a

.PHONY: all
all: clean

