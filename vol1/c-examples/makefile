# Artificial Intelligence for Humans
# Volume 1: Fundamental Algorithms
# C/C++ Version
# http://www.aifh.org
# http://www.jeffheaton.com
#
# Code repository:
# https://github.com/jeffheaton/aifh
#
# Copyright 2013 by Jeff Heaton
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# For more information on Heaton Research copyrights, licenses
# and trademarks visit:
# http://www.heatonresearch.com/copyright


# This file is based on the following tutorial

# http://www.cs.colby.edu/maxwell/courses/tutorials/maketutor/
IDIR =./
CC=gcc
CFLAGS=-I$(IDIR) -fopenmp -std=gnu99 -pedantic -O3 -Wall

ODIR=./obj
LDIR =./bin

LIBS=-lm
MKDIR_P = mkdir -p

_DEPS = csv.h aifh-vol1-examples.h
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

_OBJ = AIFH-VOL1.o ExampleNormalize.o Data.o ExamplePI.o Distance.o ExampleRandom.o Equilateral.o ExampleReadCSV.o Error.o ExampleSimpleNormalize.o ExampleAnalyze.o ExampleTest.o ExampleAnnealKnapsack.o ExampleUtil.o ExampleAnnealTSP.o KMeans.o ExampleDistance.o Normalize.o ExampleError.o RBF.o ExampleGLM.o RBFNetwork.o ExampleKMeans.o Random.o ExampleLearnIris.o Regression.o ExampleLearnIrisAnneal.o Train.o ExampleLearnIrisClimb.o asa047.o ExampleLearnIrisNelderMead.o libcsv.o ExampleLearnPoly.o matrix.o ExampleLearnXOR.o mt19937ar.o ExampleLinearRegression.o

OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.o: %.c $(DEPS)
	@mkdir -p $(@D)
	$(CC) -c -o $@ $< $(CFLAGS)

aifh-vol1: $(OBJ)
	gcc -o $@ $^ $(CFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o *~ core $(INCDIR)/*~ 
	
