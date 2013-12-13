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

/* Create the training set, and normalize the Iris data.  If you are using a data set
   other than Iris, you will need to update the normalization. */
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

This example makes use of a greedy random trainer to  to fit an RBF network to the Iris data set.  Because it is totally
random it takes 100k iterations, and can run for awhile.  The sample output below was also only able to train to 0.14,
so the results are not the best. The output is shown below.


Iteration #1: Score: 0.792750
Iteration #2: Score: 0.550081
Iteration #3: Score: 0.550081
Iteration #4: Score: 0.550081
Iteration #5: Score: 0.550081
Iteration #6: Score: 0.550081
Iteration #7: Score: 0.295338
Iteration #8: Score: 0.249367
Iteration #9: Score: 0.249367
Iteration #10: Score: 0.249367
Iteration #11: Score: 0.249367
Iteration #12: Score: 0.249367
Iteration #13: Score: 0.249367
Iteration #14: Score: 0.241296
Iteration #15: Score: 0.241296
Iteration #16: Score: 0.241296
Iteration #17: Score: 0.241296
Iteration #18: Score: 0.241296
Iteration #19: Score: 0.241296
Iteration #20: Score: 0.241296
...
Iteration #499967: Score: 0.146069
Iteration #499968: Score: 0.146069
Iteration #499969: Score: 0.146069
Iteration #499970: Score: 0.146069
Iteration #499971: Score: 0.146069
Iteration #499972: Score: 0.146069
Iteration #499973: Score: 0.146069
Iteration #499974: Score: 0.146069
Iteration #499975: Score: 0.146069
Iteration #499976: Score: 0.146069
Iteration #499977: Score: 0.146069
Iteration #499978: Score: 0.146069
Iteration #499979: Score: 0.146069
Iteration #499980: Score: 0.146069
Iteration #499981: Score: 0.146069
Iteration #499982: Score: 0.146069
Iteration #499983: Score: 0.146069
Iteration #499984: Score: 0.146069
Iteration #499985: Score: 0.146069
Iteration #499986: Score: 0.146069
Iteration #499987: Score: 0.146069
Iteration #499988: Score: 0.146069
Iteration #499989: Score: 0.146069
Iteration #499990: Score: 0.146069
Iteration #499991: Score: 0.146069
Iteration #499992: Score: 0.146069
Iteration #499993: Score: 0.146069
Iteration #499994: Score: 0.146069
Iteration #499995: Score: 0.146069
Iteration #499996: Score: 0.146069
Iteration #499997: Score: 0.146069
Iteration #499998: Score: 0.146069
Iteration #499999: Score: 0.146069
Iteration #500000: Score: 0.146069
Iteration #500001: Score: 0.146069
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
[0.72,0.46,0.66,0.58] -> Actual: [Iris-virginica], Ideal: [Iris-versicolor]
[0.33,0.12,0.51,0.50] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.61,0.33,0.61,0.58] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.39,0.33,0.59,0.50] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.56,0.54,0.63,0.62] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.17,0.17,0.39,0.38] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.64,0.37,0.61,0.50] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.25,0.29,0.49,0.54] -> Actual: [Iris-virginica], Ideal: [Iris-versicolor]
[0.19,0.00,0.42,0.38] -> Actual: [Iris-virginica], Ideal: [Iris-versicolor]
[0.44,0.42,0.54,0.58] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.47,0.08,0.51,0.38] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.50,0.37,0.63,0.54] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.36,0.37,0.44,0.50] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.67,0.46,0.58,0.54] -> Actual: [Iris-setosa], Ideal: [Iris-versicolor]
[0.36,0.42,0.59,0.58] -> Actual: [Iris-virginica], Ideal: [Iris-versicolor]
[0.42,0.29,0.53,0.38] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.53,0.08,0.59,0.58] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.36,0.21,0.49,0.42] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.44,0.50,0.64,0.71] -> Actual: [Iris-virginica], Ideal: [Iris-versicolor]
[0.50,0.33,0.51,0.50] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.56,0.21,0.66,0.58] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.50,0.33,0.63,0.46] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
[0.58,0.37,0.56,0.50] -> Actual: [Iris-versicolor], Ideal: [Iris-versicolor]
...
[0.67,0.54,0.80,1.00] -> Actual: [Iris-virginica], Ideal: [Iris-virginica]
[0.67,0.42,0.71,0.92] -> Actual: [Iris-virginica], Ideal: [Iris-virginica]
[0.56,0.21,0.68,0.75] -> Actual: [Iris-versicolor], Ideal: [Iris-virginica]
[0.61,0.42,0.71,0.79] -> Actual: [Iris-virginica], Ideal: [Iris-virginica]
[0.53,0.58,0.75,0.92] -> Actual: [Iris-virginica], Ideal: [Iris-virginica]
[0.44,0.42,0.69,0.71] -> Actual: [Iris-virginica], Ideal: [Iris-virginica]
*/
void ExampleRandIris(int argIndex, int argc, char **argv) {
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
	
	train = TrainCreateGreedyRandom(score_function,1,x0,size,params,-1,1);
	TrainRun(train,500000,0.01,1);
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