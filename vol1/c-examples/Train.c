#include "aifh-vol1.h"

void _IterationGreedyRandom(TRAIN *train) {
	int doubleSize,i;
	double *trial,trialScore;

	/* Randomize a trial position */
	doubleSize = train->position_size/sizeof(double);
	trial = (double*)train->trial_position;
	for(i=0;i<doubleSize;i++) {
		trial[i] = RandNextDoubleRange(train->random,train->low,train->high);
	}
	trialScore = train->score_function(trial,train);

	/* Did we find a better position? */
	if( TrainIsBetterThan(train, trialScore, train->best_score) ) {
		train->best_score = trialScore;
		memcpy(train->best_position,train->trial_position,train->position_size);
	}

}

void _IterationHillClimb(TRAIN *train) {
}

void _IterationAnneal(TRAIN *train) {
}

void _IterationNelderMead(TRAIN *train) {
}


TRAIN *TrainCreate(int type, SCORE_FUNCTION score_function, int should_minimize, void *x0, int position_size, void *params) {
	TRAIN *result;

	result = (TRAIN *)calloc(1,sizeof(TRAIN));
	result->type = type;
	result->low = -1;
	result->high = 1;
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
	result->best_score = score_function(result->best_position,result);
	return result;
}

void TrainDelete(TRAIN *train) {
	free(train->current_position);
	free(train->trial_position);
	free(train->best_position);
	free(train);
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
			_IterationGreedyRandom(train);
			break;
		case TYPE_TRAIN_HILL_CLIMB:
			_IterationHillClimb(train);
			break;
		case TYPE_TRAIN_ANNEAL:
			_IterationAnneal(train);
			break;
		case TYPE_TRAIN_NELDER_MEAD:
			_IterationNelderMead(train);
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
