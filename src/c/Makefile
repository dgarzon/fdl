C  = gcc
CXX = g++

INCLUDES =

CFLAGS   = -g -Wall $(INCLUDES)
CXXFLAGS = -g -Wall $(INCLUDES)

LDFLAGS = -g

LDLIBS =

# mylist-test: mylist-test.o libmylist.a

# mylist-test.o: mylist-test.c mylist.h

libmylist.a: list.o
	ar rc liblist.a list.o
	ranlib liblist.a

list.o: list.c list.h

.PHONY: clean
clean:
	rm -f *.o a.out core liblist.a

.PHONY: all
all: clean

