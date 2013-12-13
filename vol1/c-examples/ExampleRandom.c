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

static void UniformHisto(RANDOM_GENERATE *prng) {
	int i,idx,j;
	int hist[20];
	int maxIndex;
	double d;
	double started, current;
	int done;
	long total;
	double minResult = 1000;
	double maxResult = -1000;

	for(i=0;i<20;i++) {
		hist[i] = 0;
	}

	printf("Generating random numbers for 5 seconds...\nPlease wait...\n");

	started = omp_get_wtime();

	done = 0;
	total = 0;
	while(!done) {
		double d = RandNextDouble(prng);
		idx = MIN((int)(d*20),19);
		if( idx>=0 && idx<20 ) {
			hist[idx]++;
		}
		total++;

				if( d<minResult ) {
			minResult = d;
		}
		if( d>maxResult ) {
			maxResult = d;
		}

		if( total%10000 == 0 ) {
			current = (omp_get_wtime() - started);
			done = current>5;
		}
	}
	printf("Random numbers generated: %ldK\n",total/1000);
	printf("Max random number: %f\n",maxResult);
	printf("Min random number: %f\n",minResult);

	maxIndex = 1;
	for(i=1;i<20;i++) {
		if( hist[i]>hist[maxIndex] ) {
			maxIndex = i;
		}
	}

	for(i=0;i<20;i++) {
		d = ((double)hist[i])/((double)hist[maxIndex]);
		j = (int)(d*60);
		while(j>=0) {
			printf("*");
			j--;
		}
		printf("\n");
	}
}

static void NormalHisto(RANDOM_GENERATE *prng) {
	int i,idx,j;
	int hist[20];
	int maxIndex;
	double d;
	double started, current;
	int done;
	long total;
	double minResult = 1000;
	double maxResult = -1000;

	for(i=0;i<20;i++) {
		hist[i] = 0;
	}

	printf("Generating random numbers for 5 seconds...\nPlease wait...\n");

	started = omp_get_wtime();

	done = 0;
	total = 0;
	while(!done) {
		double d = RandNextGaussian(prng);
		idx = (int)((d*3)+10);
		if( idx<0 ) {
			idx = 0;
		} else if(idx>=20 ) {
			idx = 19;
		}
		if( d<minResult ) {
			minResult = d;
		}
		if( d>maxResult ) {
			maxResult = d;
		}
		hist[idx]++;
		total++;

		if( total%10000 == 0 ) {
			current = (omp_get_wtime() - started);
			done = current>5;
		}
	}

	printf("Random numbers generated: %ldK\n",total/1000);
	printf("Max random number: %f\n",maxResult);
	printf("Min random number: %f\n",minResult);

	maxIndex = 1;
	for(i=1;i<20;i++) {
		if( hist[i]>hist[maxIndex] ) {
			maxIndex = i;
		}
	}

	for(i=0;i<20;i++) {
		d = ((double)hist[i])/((double)hist[maxIndex]);
		j = (int)(d*60);
		while(j>=0) {
			printf("*");
			j--;
		}
		printf("\n");
	}
}


/*
This utility generates random numbers via several PRNG's in either normal
or uniform distribution.  Sample output:


Usage: 
random [prng] [dist]
Where prng = mt, mwc, c or lcg
and dist = uniform or normal
i.e. random mt normal
Generating random numbers for 5 seconds...
Please wait...
Random numbers generated: 383180K
Max random number: 1.000000
Min random number: 0.000000
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
*************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
*/
void ExampleRandom(int argIndex, int argc, char **argv) {	
	RANDOM_GENERATE *prng;

	if( argc-argIndex != 2 ) {
		printf("Usage: \nrandom [prng] [dist]\nWhere prng = mt, mwc, c or lcg\nand dist = uniform or normal\ni.e. random mt normal\n");
		prng = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
		UniformHisto(prng);
	} else {
		if(!strcasecmp(argv[argIndex],"c") ) {
			prng = RandCreate(TYPE_RANDOM_C,(long)time(NULL));
		} else if(!strcasecmp(argv[argIndex],"lcg") ) {
			prng = RandCreate(TYPE_RANDOM_LCG,(long)time(NULL));
		} else if(!strcasecmp(argv[argIndex],"mwc") ) {
			prng = RandCreate(TYPE_RANDOM_MWC,(long)time(NULL));
		} else if(!strcasecmp(argv[argIndex],"mt") ) {
			prng = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
		} else {
			printf("Unknown PRNG: %s\n",argv[argIndex]);
			exit(1);
		}

		if(!strcasecmp(argv[argIndex+1],"normal") ) {
			NormalHisto(prng);
		} else if(!strcasecmp(argv[argIndex+1],"uniform") ) {
			UniformHisto(prng);
		} else {
			printf("Unknown distribution: %s\n",argv[argIndex+1]);
			exit(1);
		}
	}
}