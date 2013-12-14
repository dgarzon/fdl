CC  = gcc
CXX = g++

# Include directories:
INCLUDES = -I libraries/

# Compilation options: are automatically, or implicitly called by our Makefile.
CFLAGS   = -g -Wall $(INCLUDES)
CXXFLAGS = -g -Wall $(INCLUDES)

# Linking options:
LDFLAGS = -g -L libraries/

# Libraries needed: Additional librarie
LDLIBS = -llist -lpath

# Target:
stat_calls: stat_calls.o

# Depends:
stat_calls.o: stat_calls.c

# Clean:
.PHONY: clean
clean:
		rm -f *.o *.txt a.out core stat_calls

.PHONY: all
all: clean stat_calls
