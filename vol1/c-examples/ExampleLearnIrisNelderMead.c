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
*/
void ExampleNelderMeadIris(int argIndex, int argc, char **argv) {
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

	train = TrainCreateNelderMead(score_function,x0,size,100,5000,0.0001,10,params);
	TrainRun(train,500000,0.02,1);
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