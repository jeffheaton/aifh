#include "aifh-vol1.h"

#define PHI 0x9e3779b9


/**
 * The mean for Box Muller.
 */
const double MU = 0;

/**
 * The standard deviation for Box Muller.
 */
const double SIGMA = 1;

/**
 * First part of default mod for LCG.
 */
const unsigned int LCG_DEFAULT_MOD1 = 2L;

/**
 * Second part of default mod for LCG.
 */
const unsigned int LCG_DEFAULT_MOD2 = 32L;

/**
 * Default mult for LCG.
 */
const unsigned int LCG_DEFAULT_MULT = 1103515245L;

/**
 * Default inc for LCG.
 */
const unsigned int LCG_DEFAULT_INC = 12345L;


/**
 * The maximum rand number that the standard GCC based LCG will generate.
 */
const unsigned int LCG_MAX_RAND = 4294967295L;

static int _RandLCG(RANDOM_GENERATE_LCG *gen) {
	gen->base.seed = (gen->multiplier * gen->base.seed + gen->increment)
		% gen->modulus;
	return gen->base.seed;
}

static int _RandMWC(RANDOM_GENERATE_MWC *gen) {
	uint64_t t, a = 18782LL;
	uint32_t x, r = 0xfffffffe;
	
	gen->idx = (gen->idx + 1) & 4095;
    t = a * gen->Q[gen->idx] + gen->c;
    gen->c = (t >> 32);
    x = t + gen->c;
	if (x < gen->c) {
		x++;
		gen->c++;
	}
	return (gen->Q[gen->idx] = r - x);
}

RANDOM_GENERATE *RandCreate(int type, long seed) {
	RANDOM_GENERATE *result = NULL;
	RANDOM_GENERATE_LCG *lcg = NULL;
	RANDOM_GENERATE_MWC *mwc = NULL;
	int i;

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
		case TYPE_RANDOM_MWC:
			result = (RANDOM_GENERATE *)calloc(1,sizeof(RANDOM_GENERATE_MWC));
			result->type = TYPE_RANDOM_MWC;
			mwc = (RANDOM_GENERATE_MWC *)result;
			mwc->c = 362436;
			mwc->idx = 4095;
 
			mwc->Q[0] = seed;
			mwc->Q[1] = seed + PHI;
			mwc->Q[2] = seed + PHI + PHI;
 
			for (i = 3; i < 4096; i++) {
                mwc->Q[i] = mwc->Q[i - 3] ^ mwc->Q[i - 2] ^ PHI ^ i;
			}
			break;
		case TYPE_RANDOM_MT:
			result = (RANDOM_GENERATE *)calloc(1,sizeof(RANDOM_GENERATE));
			result->type = TYPE_RANDOM_MT;
			init_genrand(seed);
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

long RandNextInt(RANDOM_GENERATE *gen) {
	switch(gen->type) {
	case TYPE_RANDOM_C:
		return rand();
	case TYPE_RANDOM_LCG:
		return _RandLCG((RANDOM_GENERATE_LCG*)gen);
	case TYPE_RANDOM_MWC:
		return _RandMWC((RANDOM_GENERATE_MWC*)gen);
	case TYPE_RANDOM_MT:
		return genrand_int31();
	default:
		printf("Invalid PRNG\n");
		exit(1);
	}	
}

double RandNextDouble(RANDOM_GENERATE *gen) {

	switch(gen->type) {
	case TYPE_RANDOM_C:
		return (double)RandNextInt(gen)/(double)RAND_MAX;
	case TYPE_RANDOM_LCG:
		return (double)RandNextInt(gen)/(double)0x7fffffff;
		case TYPE_RANDOM_MWC:
		return (double)RandNextInt(gen)/(double)0x7fffffff;
	case TYPE_RANDOM_MT:
		return (double)RandNextInt(gen)/(double)0x7fffffff;
	default:
		printf("Invalid PRNG\n");
		exit(1);
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
 
int RandNextIntRange(RANDOM_GENERATE *gen, int low, int high) {
	return ((int)(RandNextDouble(gen)*(high-low)))+low;
}

double RandNextDoubleRange(RANDOM_GENERATE *gen, double low, double high) {
	return ((double)(RandNextDouble(gen)*(high-low)))+low;
}