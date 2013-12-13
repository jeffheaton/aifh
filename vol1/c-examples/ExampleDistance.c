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

/*
This example shows how three different distance metrics calculate the same three points.

Euclidean Distance
pos1->pos2: 5.20
pos2->pos3: 5.20
pos3->pos1: 10.39

Manhattan Distance
pos1->pos2: 9.00
pos2->pos3: 9.00
pos3->pos1: 18.00

Chebyshev Distance
pos1->pos2: 3.00
pos2->pos3: 3.00
pos3->pos1: 6.00
*/
void ExampleDistance(int argIndex, int argc, char **argv) {
	double pos1[3] = { 1.0, 2.0, 3.0 };
	double pos2[3] = { 4.0, 5.0, 6.0 };
	double pos3[3] = { 7.0, 8.0, 9.0 };

	printf("Euclidean Distance\n");
	printf("pos1->pos2: %.2f\n", DistanceEuclidean(pos1,0,pos2,0,3));
	printf("pos2->pos3: %.2f\n", DistanceEuclidean(pos2,0,pos3,0,3));
	printf("pos3->pos1: %.2f\n", DistanceEuclidean(pos3,0,pos1,0,3));
	printf("\nManhattan Distance\n");
	printf("pos1->pos2: %.2f\n", DistanceManhattan(pos1,0,pos2,0,3));
	printf("pos2->pos3: %.2f\n", DistanceManhattan(pos2,0,pos3,0,3));
	printf("pos3->pos1: %.2f\n", DistanceManhattan(pos3,0,pos1,0,3));
	printf("\nChebyshev Distance\n");
	printf("pos1->pos2: %.2f\n", DistanceChebyshev(pos1,0,pos2,0,3));
	printf("pos2->pos3: %.2f\n", DistanceChebyshev(pos2,0,pos3,0,3));
	printf("pos3->pos1: %.2f\n", DistanceChebyshev(pos3,0,pos1,0,3));

}