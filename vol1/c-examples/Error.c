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
#include "aifh-vol1.h"

ERROR_CALC *ErrorCreate(int type)
{
	ERROR_CALC *result;

	result = (ERROR_CALC *)calloc(1,sizeof(ERROR_CALC));
	result->type = type;
	ErrorReset(result);
	return result;
}

void ErrorDelete(ERROR_CALC *calc) {
	free(calc);
}

void ErrorReset(ERROR_CALC *calc)
{
	calc->count = 0;
	calc->global_error = 0.0;
}

void ErrorUpdateSingle(ERROR_CALC *calc, double d1, double d2)
{
	double delta;
	delta = d1-d2;
	calc->global_error += delta*delta;
	calc->count++;
}

void ErrorUpdate(ERROR_CALC *calc, double *d1, double *d2, size_t size)
{
	size_t i;
	for(i=0;i<size;i++) {
		ErrorUpdateSingle(calc,d1[i],d2[i]);
	}
}

double ErrorCalculate(ERROR_CALC*calc)
{
	switch(calc->type) {
		case TYPE_ERROR_SSE:
			return calc->global_error;
		case TYPE_ERROR_MSE:
			return calc->global_error / calc->count;
		case TYPE_ERROR_RMS:
			return sqrt(calc->global_error / calc->count);
		default:
			printf("Unknown error calculation type\n");
			exit(1);
	}
}