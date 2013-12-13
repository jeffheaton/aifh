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

/* This example endlessly uses Montecarlo to approximate pi. 

Tries=767000000, pi=3.1415508318122556
Tries=768000000, pi=3.1415482447916667
Tries=769000000, pi=3.1415472509752926
Tries=770000000, pi=3.1415473090909090
Tries=771000000, pi=3.1415496238651102
Tries=772000000, pi=3.1415493523316060
Tries=773000000, pi=3.1415514152652007
*/
void ExamplePI(int argIndex, int argc, char **argv) {	
	RANDOM_GENERATE *prng;
	long tries;
    int i, success, lastUpdate;
	double x, y, pi;

	prng = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));

	tries = 0;
    success = 0;
    lastUpdate = 0;

    for (i = 0; i < 1000000000; i++) {
		// pick a point at random.
		x = RandNextDouble(prng);
        y = RandNextDouble(prng);

		tries++;

		// was the point inside of a circle?
        if (x * x + y * y <= 1)
			success++;

		lastUpdate++;
        if (lastUpdate >= 1000000) {
			pi = 4 * (double) success / (double) tries;
			printf("Tries=%ld, pi=%.16f\n",tries,pi);
			lastUpdate = 0;
		}
	}
}