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

/**
 * Used to produce an array of activations to classify data into groups. This
 * class is provided the number of groups, as well as the range that the
 * activations should fall into.
 * Guiver, John P., and Klimasauskas, Casimir, C. (1991).
 * "Applying Neural Networks, Part IV: Improving Performance." PC AI, July/August
 */

void Equilat (
   int classCount,
   double low,
   double high,
   double *outputMatrix
   )
{
	int i, j, k, rowSize ;
	double r, f ;
	const double min = -1;
    const double max = 1;

	rowSize = classCount - 1 ;

	/** Seed the initial matrix */
	outputMatrix[0] = -1.0 ;
	outputMatrix[rowSize] = 1.0 ;

	/* Now expand the matrix one for each class */
	for (k=2 ; k<classCount ; k++) {
		r = (double) k;
		f = sqrt ( r * r - 1.0 ) / r ; 
		for (i=0 ; i<k ; i++) {        
			for (j=0 ; j<k-1 ; j++) {     
				outputMatrix[i*rowSize+j] *= f ;
			}

			r = -1.0 / r ;
			for (i=0 ; i<k ; i++) {
				outputMatrix[i*rowSize+k-1] = r ;
			}

			for (i=0 ; i<k-1 ; i++) {
				outputMatrix[k*rowSize+i] = 0.0 ;
			}

			outputMatrix[k*rowSize+k-1] = 1.0 ;
		}
	}

	/* scale to correct range */
	for (i = 0; i < (rowSize*classCount); i++) {
		outputMatrix[i] = ((outputMatrix[i] - min) / (max - min))
                        * (high - low) + low;
	}
}
