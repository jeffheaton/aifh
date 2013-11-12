# This file is based on the following tutorial

# http://www.cs.colby.edu/maxwell/courses/tutorials/maketutor/
IDIR =./
CC=gcc
CFLAGS=-I$(IDIR)

ODIR=./obj
LDIR =./bin

LIBS=-lm
MKDIR_P = mkdir -p

_DEPS = csv.h aifh-vol1-examples.h
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

_OBJ = AIFH-VOL1.o libcsv.o ExampleReadCSV.o
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

aifh-vol1: $(OBJ)
	gcc -o $@ $^ $(CFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o *~ core $(INCDIR)/*~ 
	