#include "aifh-vol1.h"

const double NM_CCOEFF = 0.5;
const double NM_ECOEFF = 2.0;
const double NM_EPS = 0.001;
const double NM_RCOEFF = 1.0;


void _IterationGreedyRandom(TRAIN_GREEDY *train) {
	int doubleSize,i;
	double *trial,trialScore;

	/* Randomize a trial position */
	doubleSize = train->train.position_size/sizeof(double);
	trial = (double*)train->train.trial_position;
	for(i=0;i<doubleSize;i++) {
		trial[i] = RandNextDoubleRange(train->train.random,train->low,train->high);
	}
	trialScore = train->train.score_function(trial,train);

	/* Did we find a better position? */
	if( TrainIsBetterThan(&train->train, trialScore, train->train.best_score) ) {
		train->train.best_score = trialScore;
		memcpy(train->train.best_position,train->train.trial_position,train->train.position_size);
	}
}

void _IterationHillClimb(TRAIN_HILL_CLIMB *train) {
	int len, best, i, j;
	double *position, temp, bestScore;

	position = (double*)train->train.current_position;
	len = train->train.position_size / sizeof(double);

	for (i = 0; i < len; i++) {
		best = -1;
		
		for (j = 0; j < 5; j++) {
			position[i] += train->step_size[i] * train->candidate[j];
			temp = train->train.score_function(train->train.current_position, train);
            position[i] -= train->step_size[i] * train->candidate[j];
			
			if(best==-1 || TrainIsBetterThan((TRAIN*)train,temp,bestScore)) {
				bestScore = temp;
				train->train.best_score = bestScore;
                best = j;
            }
		}

		if (best != -1) {
			position[i] += train->step_size[i] * train->candidate[best];
			train->step_size[i] = train->step_size[i] * train->candidate[best];
			memcpy(train->train.best_position,train->train.trial_position,train->train.position_size);
            }
        }
}

void _IterationAnneal(TRAIN_ANNEAL *train) {
	double *position, trial_error;
	unsigned int len, cycle, keep;

	position = (double*)train->train.current_position;
	len = train->train.position_size / sizeof(double);
	train->k++;

	train->current_temperature = train->anneal_cooling(train);

        for (cycle = 0; cycle < train->cycles; cycle++) {
			/* Create a new trial position */
			memcpy(train->train.trial_position,train->train.current_position,train->train.position_size);
            /* Randomize trial state */
			train->anneal_randomize(train);

            /* did we improve it */
			trial_error = train->train.score_function(train->train.trial_position,train);

            /* was this iteration an improvement?  If so, always keep. */
            keep = 0;

			if (trial_error < train->current_score) {
                keep = 1;
            } else {

				train->lastProbability = train->anneal_probability(train->current_score, trial_error, train->current_temperature);
				if (train->lastProbability > RandNextDouble(train->train.random) ) {
                    keep = 1;
                }
            }

            if (keep) {
				train->current_score = trial_error;
                // better than global error
				if (trial_error < train->train.best_score) {
                    train->train.best_score = trial_error;
					memcpy(train->train.best_position,train->train.trial_position,train->train.position_size);				
                }
				memcpy(train->train.current_position,train->train.trial_position,train->train.position_size);
            } 
        }
}

void _IterationNelderMead(TRAIN_NELDER_MEAD *train) {
	int n, konvge, kcount;
	double *start,*xmin, *step;
	double ynewlo, reqmin;
	int icount,numres,ifault,i;

	n = train->train.position_size/sizeof(double);
	start = (double*)train->train.current_position;
	xmin = (double*)train->train.best_position;
	step = train->step;
	reqmin = train->reqmin;
	konvge = train->konvge;
	kcount = train->kcount;

	for (i = 0; i < n; i++) {
		train->step[i] = train->stepValue;
    }

	nelmin ( train->train.score_function, train, n, start, xmin, &ynewlo, reqmin, step, konvge, kcount, &icount, &numres, &ifault );
	memcpy(train->train.best_position,xmin,train->train.position_size);
	memcpy(train->train.current_position,xmin,train->train.position_size);
	train->train.best_score = ynewlo;
}


TRAIN *TrainCreateGreedyRandom(SCORE_FUNCTION score_function, int should_minimize, void *x0, int position_size, void *params,double low, double high) {
	TRAIN *result = NULL;
	TRAIN_GREEDY *trainGreedy = NULL;

	trainGreedy = (TRAIN_GREEDY*)calloc(1,sizeof(TRAIN_GREEDY));
	result = (TRAIN*)trainGreedy;
	trainGreedy->low = low;
	trainGreedy->high = high;

	result->type = TYPE_TRAIN_GREEDY_RANDOM;
	result->current_position = (unsigned char*)calloc(position_size,1);
	result->trial_position = (unsigned char*)calloc(position_size,1);
	result->best_position = (unsigned char*)calloc(position_size,1);
	result->score_function = score_function;
	result->random = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
	result->params = params;
	result->position_size = position_size;
	result->max_iterations = 0;
	result->should_minimize = should_minimize;
	memcpy(result->current_position,x0,position_size);
	memcpy(result->best_position,x0,position_size);
	result->best_score = score_function(result->best_position,result);
	return result;
}


TRAIN *TrainCreateHillClimb(SCORE_FUNCTION score_function, int should_minimize, void *x0, int position_size, double acceleration, double stepSize, void *params) {	
	TRAIN *result = NULL;
	TRAIN_HILL_CLIMB *trainHill = NULL;
	int dimensions,i;

	trainHill = (TRAIN_HILL_CLIMB*)calloc(1,sizeof(TRAIN_HILL_CLIMB));
	result = (TRAIN*)trainHill;

	result->type = TYPE_TRAIN_HILL_CLIMB;
	result->current_position = (unsigned char*)calloc(position_size,1);
	result->trial_position = (unsigned char*)calloc(position_size,1);
	result->best_position = (unsigned char*)calloc(position_size,1);
	result->score_function = score_function;
	result->random = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
	result->params = params;
	result->position_size = position_size;
	result->max_iterations = 0;
	result->should_minimize = should_minimize;
	memcpy(result->current_position,x0,position_size);
	memcpy(result->best_position,x0,position_size);
	result->best_score = score_function(result->best_position,result);

	/* Build canidate array */
	trainHill->candidate[0] = -acceleration;
    trainHill->candidate[1] = -1 / acceleration;
    trainHill->candidate[2] = 0;
    trainHill->candidate[3] = 1 / acceleration;
    trainHill->candidate[4] = acceleration;

	/* Build step sizes */
	dimensions = position_size / sizeof(double);
	trainHill->step_size = (double*)calloc(dimensions,sizeof(double));
	for(i=0;i<dimensions;i++) {
		trainHill->step_size[i] = stepSize;
	}

	return result;
}

TRAIN *TrainCreateAnneal(SCORE_FUNCTION score_function, void *x0, int position_size, double start_temperature, double stop_temperature, unsigned int cycles, unsigned int iterations, void *params)
{
	TRAIN *result = NULL;
	TRAIN_ANNEAL *trainAnneal = NULL;

	trainAnneal = (TRAIN_ANNEAL*)calloc(1,sizeof(TRAIN_ANNEAL));
	result = (TRAIN*)trainAnneal;

	result->type = TYPE_TRAIN_ANNEAL;
	result->current_position = (unsigned char*)calloc(position_size,1);
	result->trial_position = (unsigned char*)calloc(position_size,1);
	result->best_position = (unsigned char*)calloc(position_size,1);
	result->score_function = score_function;
	result->random = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
	result->params = params;
	result->position_size = position_size;
	result->max_iterations = iterations;
	result->should_minimize = 1;
	memcpy(result->current_position,x0,position_size);
	memcpy(result->best_position,x0,position_size);
	result->best_score = score_function(result->best_position,result);
	

	trainAnneal->current_temperature = start_temperature;
	trainAnneal->starting_temperature = start_temperature;
	trainAnneal->ending_temperature = stop_temperature;
	trainAnneal->k = 0;
	trainAnneal->anneal_cooling = AnnealCoolingSchedule;
	trainAnneal->anneal_probability = AnnealCalcProbability;
	trainAnneal->anneal_randomize = AnnealDoubleRandomize;
	trainAnneal->current_score = result->best_score;
	trainAnneal->cycles = cycles;

	return result;
}

TRAIN *TrainCreateNelderMead(SCORE_FUNCTION score_function, void *x0, int position_size, 
	int konvge, int kcount, double reqmin, double stepValue, void *params) {
	TRAIN *result = NULL;
	TRAIN_NELDER_MEAD *trainNM = NULL;
	unsigned int n;

	trainNM = (TRAIN_NELDER_MEAD*)calloc(1,sizeof(TRAIN_GREEDY));
	result = (TRAIN*)trainNM;
	
	result->type = TYPE_TRAIN_NELDER_MEAD;
	result->current_position = (unsigned char*)calloc(position_size,1);
	result->trial_position = (unsigned char*)calloc(position_size,1);
	result->best_position = (unsigned char*)calloc(position_size,1);
	result->score_function = score_function;
	result->random = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
	result->params = params;
	result->position_size = position_size;
	result->max_iterations = 0;
	result->should_minimize = 1;
	memcpy(result->current_position,x0,position_size);
	memcpy(result->best_position,x0,position_size);
	result->best_score = score_function(result->best_position,result);

	n = result->position_size/sizeof(double);
        
	trainNM->konvge = konvge;
	trainNM->kcount = kcount;
	trainNM->reqmin = reqmin;
	trainNM->stepValue = stepValue;
    trainNM->step = (double*)calloc(n,sizeof(double));
	return result;
}



void TrainDelete(TRAIN *train) {
	free(train->current_position);
	free(train->trial_position);
	free(train->best_position);

	switch(train->type) {
		case TYPE_TRAIN_HILL_CLIMB:
			free(((TRAIN_HILL_CLIMB*)train)->step_size);
			break;
		case TYPE_TRAIN_NELDER_MEAD:
			free(((TRAIN_NELDER_MEAD*)train)->step);
			break;
	}

	free(train);
}

void TrainComplete(TRAIN *train, void *x) {
	memcpy(x,train->best_position,train->position_size);
}

void TrainRun(TRAIN *train, int max_iterations, double target_score, int output) {
	int current_iteration;
	int done = 0;

	current_iteration = 0;
	do {
		TrainIteration(train);

		current_iteration++;
		if( output ) {
			printf("Iteration #%i: Score: %f\n",current_iteration,train->best_score);
		}

		if( current_iteration>max_iterations ) {
			done = 1;
		} else if( TrainIsBetterThan(train,train->best_score,target_score) ) {
			done = 1;
		}
	} while(!done);
}

void TrainIteration(TRAIN *train) {
	switch(train->type) {
		case TYPE_TRAIN_GREEDY_RANDOM:
			_IterationGreedyRandom((TRAIN_GREEDY*)train);
			break;
		case TYPE_TRAIN_HILL_CLIMB:
			_IterationHillClimb((TRAIN_HILL_CLIMB*)train);
			break;
		case TYPE_TRAIN_ANNEAL:
			_IterationAnneal((TRAIN_ANNEAL*)train);
			break;
		case TYPE_TRAIN_NELDER_MEAD:
			_IterationNelderMead((TRAIN_NELDER_MEAD*)train);
			break;
	}
}

int TrainIsBetterThan(TRAIN *train, double is_this, double than_that) {
	if( train->should_minimize ) {
		return is_this < than_that;
	}
	else {
		return is_this > than_that;
	}
}

double AnnealCoolingSchedule(void *a) {
	TRAIN_ANNEAL *anneal;
	double ex;
	anneal = (TRAIN_ANNEAL*)a;
	ex = (double) anneal->k / (double) anneal->train.max_iterations;
	return anneal->starting_temperature 
		* pow(anneal->ending_temperature / anneal->starting_temperature, ex);
}

void AnnealDoubleRandomize(void *a) {
	TRAIN_ANNEAL *anneal;
	int dimensions,i;
	double d, *position;

	anneal = (TRAIN_ANNEAL*)a;
	position = (double*)anneal->train.trial_position;
	dimensions = anneal->train.position_size/sizeof(double);
	for (i = 0; i < dimensions; i++) {
		d = RandNextGaussian(anneal->train.random) / 10.0;
        position[i] += d;
	}
}

double AnnealCalcProbability(double ecurrent, double enew, double t) {
	return exp(-(fabs(enew - ecurrent) / t));
}