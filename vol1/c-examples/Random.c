#include "aifh-vol1.h"


    /**
     * The mean.
     */
const double MU = 0;

    /**
     * The standard deviation.
     */
const double SIGMA = 1;

    /**
     * First part of default mod.
     */
const unsigned int LCG_DEFAULT_MOD1 = 2L;

    /**
     * Second part of default mod.
     */
const unsigned int LCG_DEFAULT_MOD2 = 32L;

    /**
     * Default mult.
     */
const unsigned int LCG_DEFAULT_MULT = 1103515245L;

    /**
     * Default inc.
     */
const unsigned int LCG_DEFAULT_INC = 12345L;


    /**
     * The maximum rand number that the standard GCC based LCG will generate.
     */
    const unsigned int LCG_MAX_RAND = 4294967295L;

int _RandLCG(RANDOM_GENERATE_LCG *gen) {
	gen->base.seed = (gen->multiplier * gen->base.seed + gen->increment)
		% gen->modulus;
	return gen->base.seed;
}


RANDOM_GENERATE *RandCreate(int type, long seed) {
	RANDOM_GENERATE *result = NULL;
	RANDOM_GENERATE_LCG *lcg = NULL;

	switch(type) {
		case TYPE_RANDOM_C:
			result = (RANDOM_GENERATE *)calloc(1,sizeof(RANDOM_GENERATE));
			result->type = TYPE_RANDOM_C;
			srand(seed);
			break;
		case TYPE_RANDOM_LCG:
			result = (RANDOM_GENERATE *)calloc(1,sizeof(RANDOM_GENERATE_LCG));
			result->type = TYPE_RANDOM_LCG;
			lcg = (RANDOM_GENERATE_LCG *)result;
			lcg->increment = LCG_DEFAULT_INC;
			lcg->modulus = (int)pow((double)LCG_DEFAULT_MOD1,(double)LCG_DEFAULT_MOD2);
			lcg->multiplier = LCG_DEFAULT_MULT;
			break;
		default:
			printf("Unknown random generator.\n");
			exit(0);
	}

	result->useLast = 0;
	result->seed = seed;

	return result;
}

void RandDelete(RANDOM_GENERATE *gen) {
	free(gen);
}

int RandNextInt(RANDOM_GENERATE *gen) {
	switch(gen->type) {
	case TYPE_RANDOM_C:
		return rand();
	case TYPE_RANDOM_LCG:
		return _RandLCG((RANDOM_GENERATE_LCG*)gen);
	}
	
}

double RandNextDouble(RANDOM_GENERATE *gen) {

	switch(gen->type) {
	case TYPE_RANDOM_C:
		return (double)RandNextInt(gen)/(double)RAND_MAX;
	case TYPE_RANDOM_LCG:
		return (double)RandNextInt(gen)/(double)LCG_MAX_RAND;
	}
}

double RandNextGaussian(RANDOM_GENERATE *gen) {
	double x1,x2,w,y1;
        
	// use value from previous call
    if (gen->useLast) {
		y1 = gen->y2;
		gen->useLast = 0;
    } else {
		do {
			x1 = 2.0 * RandNextDouble(gen) - 1.0;
			x2 = 2.0 * RandNextDouble(gen) - 1.0;
			w = x1 * x1 + x2 * x2;
		} while (w >= 1.0);

		w = sqrt((-2.0 * log(w)) / w);
		y1 = x1 * w;
		gen->y2 = x2 * w;
		gen->useLast = 1;
	}

    return (MU + y1 * SIGMA);
}