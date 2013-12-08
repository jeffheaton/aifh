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

#define NUM_ITEMS_TO_CHOOSE 25
#define KNAPSACK_MAX_WEIGHT 50
#define ITEM_MAX_WEIGHT 20
#define ITEM_MAX_VALUE 1000

typedef struct KNAPSACK_PARAMS {
	double profit[NUM_ITEMS_TO_CHOOSE];
	double weight[NUM_ITEMS_TO_CHOOSE];
	RANDOM_GENERATE *random;
} KNAPSACK_PARAMS;

static KNAPSACK_PARAMS *CreateKnapsack() {
	KNAPSACK_PARAMS *result;
	int n;

	result = (KNAPSACK_PARAMS *)calloc(1,sizeof(KNAPSACK_PARAMS));
	result->random = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));

	/* Generate a random set of items. */
	for (n = 0; n < NUM_ITEMS_TO_CHOOSE; n++) {
		result->profit[n] = RandNextIntRange(result->random,0, ITEM_MAX_VALUE);
		result->weight[n] = RandNextIntRange(result->random,0, ITEM_MAX_WEIGHT);
	}

	return result;
}

int CalculateTotalWeight(KNAPSACK_PARAMS *params, unsigned char *items, int item_count) {
	int i;
	double result;

	result = 0;
	for (i = 0; i < item_count; i++) {
		if (items[i]) {
			result += params->weight[i];
		}
	}
        
	return (int)result;    
}

static void Balance(KNAPSACK_PARAMS *params, unsigned char *items, int item_count) {
	int remove;
	while (CalculateTotalWeight(params,items,item_count) > KNAPSACK_MAX_WEIGHT) {
		remove = RandNextIntRange(params->random, 0, item_count);
		items[remove] = 0;
	}
}

void KnapsackRandomize(void *a) {
	TRAIN_ANNEAL *anneal;
	int dimensions,i,  holdingEverythingAlready, pt;
	unsigned char *position;

	anneal = (TRAIN_ANNEAL*)a;
	position = (unsigned char*)anneal->train.trial_position;
	dimensions = anneal->train.position_size/sizeof(unsigned char);

	/* check for strange case where we have everything!
		This means that the max allowed knapsack weight is greater than the total of grabbing everything.
		This is kind of pointless, but don't go into an endless loop! */
	holdingEverythingAlready = 1;
	for (i=0;i<dimensions;i++) {
		if (!position[i]) {
			holdingEverythingAlready = 0;
			break;
		}
	}

	if (!holdingEverythingAlready) {
		/* try to add something */
		pt = RandNextIntRange(anneal->train.random, 0, dimensions); /* prime */
		while (position[pt]) {
			pt = RandNextIntRange(anneal->train.random, 0, dimensions);
		}

		/* add the item we found */
		position[pt] = 1;

		/* We probably need to drop something now. */
		Balance((KNAPSACK_PARAMS*)anneal->train.params, position, dimensions);
	}
}

static double score_function(void *m, void *p) {
	TRAIN *train;
	KNAPSACK_PARAMS *params;
	unsigned char *memory;
	unsigned int i;
	double result = 0;
	
	memory = (unsigned char*)m;
	train = (TRAIN*)p;
	params = (KNAPSACK_PARAMS *)train->params;

	if (CalculateTotalWeight(params,memory,train->position_size) > KNAPSACK_MAX_WEIGHT) {
		return 1000000.0; /* bad socre */
	}

	for (i = 0; i < train->position_size; i++) {
		if (memory[i]) {
			result += params->profit[i];
		}
	}
	return result;
}

void RandomFill(unsigned char *items) {
	int i;
	RANDOM_GENERATE *prng;
	prng = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
	for(i=0;i<NUM_ITEMS_TO_CHOOSE;i++) {
		items[i] = RandNextDouble(prng)>0.5;
	}
}

void ExampleAnnealKnapsack(int argIndex, int argc, char **argv) {
	KNAPSACK_PARAMS *params;
	TRAIN *train;
	unsigned char x0[NUM_ITEMS_TO_CHOOSE],*x;
	unsigned int size, n;
	const int kMax = 100;

	params = CreateKnapsack();
	size = NUM_ITEMS_TO_CHOOSE * sizeof(unsigned char);
	
	RandomFill(x0);
	train = TrainCreateAnneal(score_function,x0,size,400,0.0001,500,kMax,params);
	((TRAIN_ANNEAL*)train)->anneal_randomize = KnapsackRandomize;
	TrainRun(train,kMax,0.01,1);

	/* print results */
	x = (unsigned char*)train->best_position;
    printf("item\tprofit\tweight\n");
    for (n = 0; n < NUM_ITEMS_TO_CHOOSE; n++) {
		if(x[n]) {
			printf("%i\t%.2f\t%.2f\n", (n + 1), params->profit[n], params->weight[n]);
		}
    }
	
	free(params);
}