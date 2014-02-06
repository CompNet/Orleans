#!/bin/bash

CC=g++

SRCDIR=src
HEADDIR=include
LIBDIR=obj
BINDIR=bin

CFLAGS= -ansi -O5 -Wall  -fopenmp
LDFLAGS= -ansi -lm -Wall  -fopenmp
EXEC=convert guimera overlap

SRC= $(wildcard $(SRCDIR)/*.cpp)
OBJ1= $(SRC:$(SRCDIR)/%.cpp=$(LIBDIR)/graph_binary.o)
OBJ2= $(SRC:$(SRCDIR)/%.cpp=$(LIBDIR)/graph.o)
OBJ3 = $(SRC:$(SRCDIR)/%.cpp=$(LIBDIR)/graph.o) $(SRC:$(SRCDIR)/%.cpp=$(LIBDIR)/graph_binary.o)

all: $(EXEC)

convert : $(OBJ2) $(SRC:$(SRCDIR)/%.cpp=$(LIBDIR)/main_convert.o)
	$(CC) -g -o $(BINDIR)/$@ $^ $(LDFLAGS)

guimera : $(OBJ3) $(SRC:$(SRCDIR)/%.cpp=$(LIBDIR)/main_guimera.o)
	$(CC) -g -o $(BINDIR)/$@ $^ $(LDFLAGS)

overlap : $(OBJ3) $(SRC:$(SRCDIR)/%.cpp=$(LIBDIR)/main_overlap.o)
	$(CC) -g -o $(BINDIR)/$@ $^ $(LDFLAGS)

##########################################
# Generic rules
##########################################

$(LIBDIR)/%.o: $(SRCDIR)/%%.cpp $(HEADDIR)/%.h
	$(CC) -g -o $@ -c $< $(CFLAGS)

$(LIBDIR)/%.o: $(SRCDIR)/%.cpp
	$(CC) -g -o $@ -c $< $(CFLAGS)

clean:
	rm -f $(LIBDIR)/*.o $(LIBDIR)/*~ $(SRCDIR)/*~ 
