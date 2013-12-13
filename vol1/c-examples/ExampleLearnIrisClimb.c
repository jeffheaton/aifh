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

/* Paramaters for Iris training.  Includes the training data, the error calculation 
   method, as well as the RBF network model to train. */
typedef struct IRIS_PARAMS {
	DATA_SET *training;
	ERROR_CALC *errorCalc;
	RBF_NETWORK *network;
} IRIS_PARAMS;

static DATA_SET *create_iris_training(NORM_DATA *norm) {
	char filename[FILENAME_MAX];
	
	DATA_SET *data;

	LocateFile("iris.csv",filename,FILENAME_MAX);
	
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefClass(norm,NORM_CLASS_ONEOFN,0,1);

	NormAnalyze(norm,filename);
	data = NormProcess(norm,filename,4,1);	
	return data;
}

/* The score function uses an error calculation method to provide a score to be minimized.
    The actual output from the model is compared against the training data. */
static double score_function(void *m, void *p) {
	IRIS_PARAMS *params;
	RBF_NETWORK *network;
	TRAIN *train;
	double *input, *ideal,y[3];
	unsigned int row;
	double *memory;
	
	memory = (double*)m;
	train = (TRAIN*)p;
	params = (IRIS_PARAMS *)train->params;
	network = params->network;

	memcpy(network->long_term_memory,m,sizeof(double)*network->ltm_size);

	ErrorReset(params->errorCalc);
	for(row=0;row<params->training->recordCount;row++) {
		input = DataGetInput(params->training,row);
		ideal = DataGetIdeal(params->training,row);
		RBFNetworkComputeRegression(network, input, y);
		ErrorUpdate(params->errorCalc,y,ideal,3);
	}
	return ErrorCalculate(params->errorCalc);
}

/*
This example uses hill climbing to fit a RBF network to the Iris data set.  Hill climbing will give very mixed results.
My example below trained below 0.11 in 101 iterations.  Sometimes it will be much better.  I've seen examples train to under 0.01
in only a handful of iterations.   This is because the initial random position of the RBF network was pretty good.  Often you 
ßwill need considerably more iterations for a considerably worse outcome.

Iteration #1: Score: 0.193933
Iteration #2: Score: 0.155749
Iteration #3: Score: 0.138598
Iteration #4: Score: 0.138598
Iteration #5: Score: 0.138598
Iteration #6: Score: 0.138598
Iteration #7: Score: 0.138598
Iteration #8: Score: 0.134825
Iteration #9: Score: 0.111405
Iteration #10: Score: 0.106551
Iteration #11: Score: 0.106551
Iteration #12: Score: 0.106551
Iteration #13: Score: 0.106551
Iteration #14: Score: 0.106551
Iteration #15: Score: 0.106551
Iteration #16: Score: 0.106551
Iteration #17: Score: 0.106551
Iteration #18: Score: 0.106551
Iteration #19: Score: 0.106551
Iteration #20: Score: 0.106551
Iteration #21: Score: 0.106551
Iteration #22: Score: 0.106551
Iteration #23: Score: 0.106551
Iteration #24: Score: 0.106551
Iteration #25: Score: 0.106551
Iteration #26: Score: 0.106551
Iteration #27: Score: 0.106551
Iteration #28: Score: 0.106551
Iteration #29: Score: 0.106551
Iteration #30: Score: 0.106551
Iteration #31: Score: 0.106551
Iteration #32: Score: 0.106551
Iteration #33: Score: 0.106551
Iteration #34: Score: 0.106551
Iteration #35: Score: 0.106551
Iteration #36: Score: 0.106551
Iteration #37: Score: 0.106551
Iteration #38: Score: 0.106551
Iteration #39: Score: 0.106551
Iteration #40: Score: 0.106551
Iteration #41: Score: 0.106551
Iteration #42: Score: 0.106551
Iteration #43: Score: 0.106551
Iteration #44: Score: 0.106551
Iteration #45: Score: 0.106551
Iteration #46: Score: 0.106551
Iteration #47: Score: 0.106551
Iteration #48: Score: 0.106551
Iteration #49: Score: 0.106551
Iteration #50: Score: 0.106551
Iteration #51: Score: 0.106551
Iteration #52: Score: 0.106551
Iteration #53: Score: 0.106551
Iteration #54: Score: 0.106551
Iteration #55: Score: 0.106551
Iteration #56: Score: 0.106551
Iteration #57: Score: 0.106551
Iteration #58: Score: 0.106551
Iteration #59: Score: 0.106551
Iteration #60: Score: 0.106551
Iteration #61: Score: 0.106551
Iteration #62: Score: 0.106551
Iteration #63: Score: 0.106551
Iteration #64: Score: 0.106551
Iteration #65: Score: 0.106551
Iteration #66: Score: 0.106551
Iteration #67: Score: 0.106551
Iteration #68: Score: 0.106551
Iteration #69: Score: 0.106551
Iteration #70: Score: 0.106551
Iteration #71: Score: 0.106551
Iteration #72: Score: 0.106551
Iteration #73: Score: 0.106551
Iteration #74: Score: 0.106551
Iteration #75: Score: 0.106551
Iteration #76: Score: 0.106551
Iteration #77: Score: 0.106551
Iteration #78: Score: 0.106551
Iteration #79: Score: 0.106551
Iteration #80: Score: 0.106551
Iteration #81: Score: 0.106551
Iteration #82: Score: 0.106551
Iteration #83: Score: 0.106551
Iteration #84: Score: 0.106551
Iteration #85: Score: 0.106551
Iteration #86: Score: 0.106551
Iteration #87: Score: 0.106551
Iteration #88: Score: 0.106551
Iteration #89: Score: 0.106551
Iteration #90: Score: 0.106551
Iteration #91: Score: 0.106551
Iteration #92: Score: 0.106551
Iteration #93: Score: 0.106551
Iteration #94: Score: 0.106551
Iteration #95: Score: 0.106551
Iteration #96: Score: 0.106551
Iteration #97: Score: 0.106551
Iteration #98: Score: 0.106551
Iteration #99: Score: 0.106551
Iteration #100: Score: 0.106551
Iteration #101: Score: 0.106551
[0.22,0.62,0.07,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.17,0.42,0.07,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.11,0.50,0.05,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.08,0.46,0.08,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.19,0.67,0.07,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.31,0.79,0.12,0.13] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.08,0.58,0.07,0.08] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.19,0.58,0.08,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.03,0.37,0.07,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.17,0.46,0.08,0.00] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.31,0.71,0.08,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.14,0.58,0.10,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.14,0.42,0.07,0.00] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.00,0.42,0.02,0.00] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.42,0.83,0.03,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.39,1.00,0.08,0.13] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.31,0.79,0.05,0.13] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.22,0.62,0.07,0.08] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.39,0.75,0.12,0.08] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.22,0.75,0.08,0.08] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.31,0.58,0.12,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.22,0.71,0.08,0.13] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.08,0.67,0.00,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.22,0.54,0.12,0.17] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.14,0.58,0.15,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.19,0.42,0.10,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.19,0.58,0.10,0.13] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.25,0.62,0.08,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.25,0.58,0.07,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.11,0.50,0.10,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.14,0.46,0.10,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.31,0.58,0.08,0.13] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.25,0.87,0.08,0.00] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.33,0.92,0.07,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.17,0.46,0.08,0.00] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.19,0.50,0.03,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.33,0.62,0.05,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.17,0.46,0.08,0.00] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.03,0.42,0.05,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.22,0.58,0.08,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.19,0.62,0.05,0.08] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.06,0.12,0.05,0.08] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.03,0.50,0.05,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.19,0.62,0.10,0.21] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.22,0.75,0.15,0.13] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.14,0.42,0.07,0.08] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.22,0.75,0.10,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.08,0.50,0.07,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.28,0.71,0.08,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.19,0.54,0.07,0.04] -> Actual: [Iris-setosa], Ideal: [Iris-setosa]
[0.75,0.50,0.63,0.54] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.58,0.50,0.59,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.72,0.46,0.66,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.33,0.12,0.51,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.61,0.33,0.61,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.39,0.33,0.59,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.56,0.54,0.63,0.62] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.17,0.17,0.39,0.38] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.64,0.37,0.61,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.25,0.29,0.49,0.54] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.19,0.00,0.42,0.38] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.44,0.42,0.54,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.47,0.08,0.51,0.38] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.50,0.37,0.63,0.54] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.36,0.37,0.44,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.67,0.46,0.58,0.54] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.36,0.42,0.59,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.42,0.29,0.53,0.38] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.53,0.08,0.59,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.36,0.21,0.49,0.42] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.44,0.50,0.64,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.50,0.33,0.51,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.56,0.21,0.66,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.50,0.33,0.63,0.46] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.58,0.37,0.56,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.64,0.42,0.58,0.54] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.69,0.33,0.64,0.54] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.67,0.42,0.68,0.67] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.47,0.37,0.59,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.39,0.25,0.42,0.38] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.33,0.17,0.47,0.42] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.33,0.17,0.46,0.38] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.42,0.29,0.49,0.46] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.47,0.29,0.69,0.62] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.31,0.42,0.59,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.47,0.58,0.59,0.62] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.67,0.46,0.63,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.56,0.12,0.58,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.36,0.42,0.53,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.33,0.21,0.51,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.33,0.25,0.58,0.46] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.50,0.42,0.61,0.54] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.42,0.25,0.51,0.46] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.19,0.12,0.39,0.38] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.36,0.29,0.54,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.39,0.42,0.54,0.46] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.39,0.37,0.54,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.53,0.37,0.56,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.22,0.21,0.34,0.42] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.39,0.33,0.53,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.56,0.54,0.85,1.00] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.42,0.29,0.69,0.75] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.78,0.42,0.83,0.83] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.56,0.37,0.78,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.61,0.42,0.81,0.88] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.92,0.42,0.95,0.83] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.17,0.21,0.59,0.67] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.83,0.37,0.90,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.67,0.21,0.81,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.81,0.67,0.86,1.00] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.61,0.50,0.69,0.79] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.58,0.29,0.73,0.75] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.69,0.42,0.76,0.83] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.39,0.21,0.68,0.79] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.42,0.33,0.69,0.96] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.58,0.50,0.73,0.92] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.61,0.42,0.76,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.94,0.75,0.97,0.88] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.94,0.25,1.00,0.92] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.47,0.08,0.68,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.72,0.50,0.80,0.92] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.36,0.33,0.66,0.79] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.94,0.33,0.97,0.79] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.56,0.29,0.66,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.67,0.54,0.80,0.83] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.81,0.50,0.85,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.53,0.33,0.64,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.50,0.42,0.66,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.58,0.33,0.78,0.83] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.81,0.42,0.81,0.62] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.86,0.33,0.86,0.75] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[1.00,0.75,0.92,0.79] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.58,0.33,0.78,0.88] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.56,0.33,0.69,0.58] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.50,0.25,0.78,0.54] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.94,0.42,0.86,0.92] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.56,0.58,0.78,0.96] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.58,0.46,0.76,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.47,0.42,0.64,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.72,0.46,0.75,0.83] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.67,0.46,0.78,0.96] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.72,0.46,0.69,0.92] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.42,0.29,0.69,0.75] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.69,0.50,0.83,0.92] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.67,0.54,0.80,1.00] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.67,0.42,0.71,0.92] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.56,0.21,0.68,0.75] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.61,0.42,0.71,0.79] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.53,0.58,0.75,0.92] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]
[0.44,0.42,0.69,0.71] -> Actual: [Iris-setosa], Ideal: [Iris-virginica]

*/
void ExampleHillClimbIris(int argIndex, int argc, char **argv) {
	IRIS_PARAMS *params;
	NORM_DATA *norm;
	TRAIN *train;
	double *x0, *input, *ideal,y[3];
	unsigned int size,i;
	NORM_DATA_ITEM *irisSpecies;
	char *idealSpecies,*actualSpecies;

	params = (IRIS_PARAMS*)calloc(1,sizeof(IRIS_PARAMS));
	norm = NormCreate();
	params->training = create_iris_training(norm);
	params->errorCalc = ErrorCreate(TYPE_ERROR_MSE);
	params->network = RBFNetworkCreate(RBFGaussian,4,5,3);
	RBFNetworkReset(params->network);

	size = params->network->ltm_size*sizeof(double);
	x0 = (double*)calloc(size,1);
	memcpy(x0,params->network->long_term_memory,size);

	/* Extract the species definition */
	irisSpecies = NormGetColumnItem(norm, 4);
	
	train = TrainCreateHillClimb(score_function,1,x0,size,1.2,1.0,params);
	TrainRun(train,100,0.01,1);
	TrainComplete(train,params->network->long_term_memory);

	/* Perform the evaluation */
	for(i=0;i<params->training->recordCount;i++) {
		input = DataGetInput(params->training,i);
		ideal = DataGetIdeal(params->training,i);
		RBFNetworkComputeRegression(params->network, input, y);
		idealSpecies = DeNormOneOfN(irisSpecies->firstClass,irisSpecies->targetLow,irisSpecies->targetLow,ideal);
		actualSpecies = DeNormOneOfN(irisSpecies->firstClass,irisSpecies->targetLow,irisSpecies->targetLow,y);
		printf("[%.2f,%.2f,%.2f,%.2f] -> Actual: [%s], Ideal: [%s]\n",
			input[0],input[1],input[2],input[3],actualSpecies,idealSpecies);
	}
	
	DataDelete(params->training);
	ErrorDelete(params->errorCalc);
	RBFNetworkDelete(params->network);
	NormDelete(norm);
	free(params);
}