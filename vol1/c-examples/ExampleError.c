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

/**
 * The random seed to use.
 */
const int SEED = 1420;

/**
 * The number of rows.
 */
const int ROWS = 10000;

/**
 * The number of columns.
 */
const int COLS = 25;

/**
 * The low value.
 */
const double LOW = -1;

/**
 * The high value.
 */
const double HIGH = 1;

/* Generate random actual and ideal data.  The actual data is calculated by distorting
   the ideal data.  This simulates actual error that real models would produce. 
   */
double *generate(int seed, int rows, int cols, double low, double high, double distort) {
        RANDOM_GENERATE *prng;
		double *result, *row_ptr;
		int row, col, row_size, total_size;
		
		prng = RandCreate(TYPE_RANDOM_MT,(long)seed);
		row_size = cols * 2;
		total_size = row_size * rows;

		result = (double*)calloc(row_size*rows,sizeof(double));

        for (row = 0; row < rows; row++) {
			row_ptr = result + (row_size*row);
            for (col = 0; col < cols; col++) {
				row_ptr[col] = RandNextDoubleRange(prng,low,high);
				row_ptr[col+cols] = row_ptr[col] + (RandNextGaussian(prng)*distort);
            }
        }

        return result;
}

/* Calculate the error between the ideal and actual. */
double calculate_error(double *data, int rows, int cols, int type) {
	ERROR_CALC *calc;
	double result,*current_row;
	int row;
	
	calc = ErrorCreate(type);

	for(row=0;row<rows;row++) {
		current_row = data + ((cols*2) * row);
		ErrorUpdate(calc,current_row,current_row+cols,cols);
	}

	result = ErrorCalculate(calc);
	ErrorDelete(calc);

	return result;
}

/*
This example shows how three different error metrics calculate different data sets.

Type	SSE		MSE		RMS
Small	2486		0.009945	0.099727
Medium	62159		0.248637	0.498635
Large	248636		0.994548	0.997270
Hughe	24863690	99.454763	9.972701
*/
void ExampleError(int argIndex, int argc, char **argv) {
	double *smallErrors,*mediumErrors,*largeErrors,*hugeErrors;
	double smallSSE, smallMSE, smallRMS;
	double mediumSSE, mediumMSE, mediumRMS;
	double largeSSE, largeMSE, largeRMS;
	double hugeSSE, hugeMSE, hugeRMS;

	smallErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 0.1);
    mediumErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 0.5);
    largeErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 1.0);
    hugeErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 10.0);

	smallSSE = calculate_error(smallErrors,ROWS, COLS, TYPE_ERROR_SSE);
	smallMSE = calculate_error(smallErrors,ROWS, COLS, TYPE_ERROR_MSE);
    smallRMS = calculate_error(smallErrors,ROWS, COLS, TYPE_ERROR_RMS);

    mediumSSE = calculate_error(mediumErrors,ROWS, COLS, TYPE_ERROR_SSE);
    mediumMSE = calculate_error(mediumErrors,ROWS, COLS, TYPE_ERROR_MSE);
    mediumRMS = calculate_error(mediumErrors,ROWS, COLS, TYPE_ERROR_RMS);

    largeSSE = calculate_error(largeErrors,ROWS, COLS, TYPE_ERROR_SSE);
    largeMSE = calculate_error(largeErrors,ROWS, COLS, TYPE_ERROR_MSE);
    largeRMS = calculate_error(largeErrors,ROWS, COLS, TYPE_ERROR_RMS);

    hugeSSE = calculate_error(hugeErrors,ROWS, COLS, TYPE_ERROR_SSE);
    hugeMSE = calculate_error(hugeErrors,ROWS, COLS, TYPE_ERROR_MSE);
    hugeRMS = calculate_error(hugeErrors,ROWS, COLS, TYPE_ERROR_RMS);

	printf("Type\tSSE\t\tMSE\t\tRMS\n");
	printf("Small\t%d\t\t%f\t%f\n",(int)smallSSE,smallMSE,smallRMS);
	printf("Medium\t%d\t\t%f\t%f\n",(int)mediumSSE,mediumMSE,mediumRMS);
	printf("Large\t%d\t\t%f\t%f\n",(int)largeSSE,largeMSE,largeRMS);
	printf("Hughe\t%d\t%f\t%f\n",(int)hugeSSE,hugeMSE,hugeRMS);



	free(smallErrors);
    free(mediumErrors);
    free(largeErrors);
    free(hugeErrors);
}