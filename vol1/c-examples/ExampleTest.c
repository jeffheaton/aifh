#include "aifh-vol1-examples.h"


void ExampleTest(int argIndex, int argc, char **argv) {	
	RANDOM_GENERATE *prng;
	int i;

	prng = RandCreate(TYPE_RANDOM_MT,(unsigned)time(NULL));

	for(i=0;i<10;i++) {
		printf("%ld\n",RandNextInt(prng));
	}
}