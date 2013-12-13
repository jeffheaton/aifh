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

typedef struct POLY_PARAMS {
	DATA_SET *training;
	ERROR_CALC *errorCalc;
} POLY_PARAMS;

/* Create the training data.  We do this by running a range of numbers [-50 to 50]
   through a known polynomial.  */
static DATA_SET *create_poly_training() {
	DATA_SET *result;
	double *input_data;
	double *ideal_data;
	int i, idx;

	result = DataCreate(101,1,1);
	idx = 0;
	for(i=-50;i<=50;i++) {
		input_data = DataGetInput(result,idx);
		ideal_data = DataGetIdeal(result,idx);
		*input_data = (double)i;
		*ideal_data = (pow(*input_data,2)*6)+(*input_data*4)+2;
		idx++;
	}

	return result;
}

/* The score function uses an error calculation method to provide a score to be minimized.
    The actual output from the model is compared against the training data. */
static double score_function(void *m, void *p) {
	POLY_PARAMS *params;
	TRAIN *train;
	double *input, *ideal,y;
	unsigned int row;
	double *memory;
	
	memory = (double*)m;
	train = (TRAIN*)p;
	params = (POLY_PARAMS *)train->params;

	ErrorReset(params->errorCalc);
	for(row=0;row<params->training->recordCount;row++) {
		input = DataGetInput(params->training,row);
		ideal = DataGetIdeal(params->training,row);
		y = (pow(*input,2)*memory[0])+(*input*memory[1])+memory[2];
		ErrorUpdate(params->errorCalc,&y,ideal,1);
	}
	return ErrorCalculate(params->errorCalc);
}

static void print_polynomial(double *coeff, int len) {
	int i;
	double c;
	int started = 0;

	for (i = len - 1; i >= 0; i--) {
		c = coeff[i];

		if (started) {
			if (c >= 0) {
				printf("+");
			}
		}

		printf("%f",c);

		if (i >= 2) {
			printf("x^%i",i);
		} else if (i >= 1) {
			printf("x");
		}

		started = 1;
	}
	printf("\n");
}

/*
This example uses a greedy random trainer to fit the coefficients of a 3rd degree polynomial.  Because it is totally
random it takes 100k iterations, and can run for awhile.  The output is shown below.

The ideal coefficients would be:
6+4x+2x^2

However, the final output is reasonably close:

Iteration #1: Score: 3097906018.729971
Iteration #2: Score: 3097906018.729971
Iteration #3: Score: 768008686.518420
Iteration #4: Score: 768008686.518420
Iteration #5: Score: 768008686.518420
Iteration #6: Score: 768008686.518420
Iteration #7: Score: 580186260.639532
Iteration #8: Score: 580186260.639532
Iteration #9: Score: 580186260.639532
Iteration #10: Score: 580186260.639532
...
Iteration #999999: Score: 606.908330
Iteration #1000000: Score: 606.908330
Iteration #1000001: Score: 606.908330
5.160841x^2+4.034914x+5.998574

*/
void ExamplePoly(int argIndex, int argc, char **argv) {
	double test[3] = { 0,0,0 };
	POLY_PARAMS *params;
	TRAIN *train;

	params = (POLY_PARAMS*)calloc(1,sizeof(POLY_PARAMS));
	params->training = create_poly_training();
	params->errorCalc = ErrorCreate(TYPE_ERROR_SSE);
	
	train = TrainCreateGreedyRandom(score_function,1,test,sizeof(double)*3,params,-10,10);
	TrainRun(train,1000000,0.01,1);
	
	DataDelete(params->training);
	ErrorDelete(params->errorCalc);
	print_polynomial((double*)train->best_position,3);
	free(params);
}