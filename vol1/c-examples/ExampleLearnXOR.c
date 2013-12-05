/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * C/C++ Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
#include "aifh-vol1-examples.h"

typedef struct XOR_PARAMS {
	DATA_SET *training;
	ERROR_CALC *errorCalc;
	RBF_NETWORK *network;
} POLY_PARAMS;

static DATA_SET *create_xor_training() {
	DATA_SET *result;

	result = DataCreate(4,2,1);
	DataMoveCursor(result,0);
	DataAddVar(result,0,0,0);
	DataAddVar(result,1,0,1);
	DataAddVar(result,0,1,1);
	DataAddVar(result,1,1,0);

	return result;
}

static double score_function(void *m, void *p) {
	POLY_PARAMS *params;
	TRAIN *train;
	double result,*input, *ideal,y;
	int row;
	double *memory;
	
	memory = (double*)m;
	train = (TRAIN*)p;
	params = (XOR_PARAMS *)train->params;

	ErrorReset(params->errorCalc);
	for(row=0;row<params->training->recordCount;row++) {
		input = DataGetInput(params->training,row);
		ideal = DataGetIdeal(params->training,row);
		y = (pow(*input,2)*memory[0])+(*input*memory[1])+memory[2];
		ErrorUpdate(params->errorCalc,&y,ideal,1);
	}
	return ErrorCalculate(params->errorCalc);
}

void ExampleRandXOR(int argIndex, int argc, char **argv) {
	XOR_PARAMS *params;
	TRAIN *train;
	double *x0;

	params = (POLY_PARAMS*)calloc(1,sizeof(POLY_PARAMS));
	params->training = create_xor_training();
	params->errorCalc = ErrorCreate(TYPE_ERROR_SSE);
	params->network = RBFNetworkCreate(RBFGaussian,2,5,1);
	RBFNetworkReset(params->network);

	x0 = (double*)calloc(params->network->ltm_size,sizeof(double));
	memcpy(x0,params->network->long_term_memory,(params->network->ltm_size*sizeof(double));
	
	train = TrainCreate(TYPE_TRAIN_GREEDY_RANDOM,score_function,1,x0,sizeof(double)*3,params);
	train->low = -10;
	train->high = 10;
	TrainRun(train,1000000,0.01,1);
	
	DataDelete(params->training);
	ErrorDelete(params->errorCalc);
	RBFNetworkDelete(params->network);
	free(params);
}