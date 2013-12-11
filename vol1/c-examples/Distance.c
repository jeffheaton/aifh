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

#include <math.h>

double DistanceEuclidean(
	double *position1, 
	int pos1, 
	double *position2, 
	int pos2, 
	int length) {
        double sum = 0,d;
		int i;
        
		for (i = 0; i < length; i++) {
            d = position1[i + pos1] - position2[i + pos2];
            sum += d * d;
        }
        return sqrt(sum);
    }

double DistanceManhattan(
	double *position1, 
	int pos1, 
	double *position2, 
	int pos2, 
	int length) {
        double sum = 0, d;
		int i;

        for (i = 0; i < length; i++) {
            d = fabs(position1[i+pos1] - position2[i+pos2]);
            sum += d;
        }
        return sum;
    }

double DistanceChebyshev(
	double *position1, 
	int pos1, 
	double *position2, 
	int pos2, 
	int length) {
        double result = 0, d;
		int i;

        for (i = 0; i < length; i++) {
            d = fabs(position1[pos1 + i] - position2[pos2 + i]);
            result = MAX(d, result);
        }
        return result;
    }