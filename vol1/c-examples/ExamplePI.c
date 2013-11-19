#include "aifh-vol1-examples.h"

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
			printf("Tries=%i, pi=%.16f\n",tries,pi);
			lastUpdate = 0;
		}
	}
}