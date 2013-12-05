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


void ExampleRandom(int argIndex, int argc, char **argv) {	
	RANDOM_GENERATE *prng;

	if( argc-argIndex != 2 ) {
		printf("Usage: \nrandom [prng] [dist]\nWhere prng = mt, mwc, c or lcg\nand dist = uniform or normal\ni.e. random mt normal\n");
		prng = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
		UniformHisto(prng);
	} else {
		if(!strcmpi(argv[argIndex],"c") ) {
			prng = RandCreate(TYPE_RANDOM_C,(long)time(NULL));
		} else if(!strcmpi(argv[argIndex],"lcg") ) {
			prng = RandCreate(TYPE_RANDOM_LCG,(long)time(NULL));
		} else if(!strcmpi(argv[argIndex],"mwc") ) {
			prng = RandCreate(TYPE_RANDOM_MWC,(long)time(NULL));
		} else if(!strcmpi(argv[argIndex],"mt") ) {
			prng = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
		} else {
			printf("Unknown PRNG: %s\n",argv[argIndex]);
			exit(1);
		}

		if(!strcmpi(argv[argIndex+1],"normal") ) {
			NormalHisto(prng);
		} else if(!strcmpi(argv[argIndex+1],"uniform") ) {
			UniformHisto(prng);
		} else {
			printf("Unknown distribution: %s\n",argv[argIndex+1]);
			exit(1);
		}
	}

	

	printf("%i\n",sizeof(int));
}