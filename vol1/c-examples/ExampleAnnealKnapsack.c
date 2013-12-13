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

/* The number of items to choose from for the knapsack. */
#define NUM_ITEMS_TO_CHOOSE 100

/* The maximum weight that the knapsack can hold */
#define KNAPSACK_MAX_WEIGHT 50

/* The maximum weight of a single item. */
#define ITEM_MAX_WEIGHT 20

/* The maximum value of a single item. */
#define ITEM_MAX_VALUE 150

/* Holds the basic paramaters for the knapsack problem.  Also holds the random
   number generator to be used. */
typedef struct KNAPSACK_PARAMS {
	double profit[NUM_ITEMS_TO_CHOOSE];
	double weight[NUM_ITEMS_TO_CHOOSE];
	double max_profit;
	RANDOM_GENERATE *random;
} KNAPSACK_PARAMS;

/* Create an instance of the knapsack problem */
static KNAPSACK_PARAMS *CreateKnapsack() {
	KNAPSACK_PARAMS *result;
	int n;

	/* Create the KNAPSACK_PARAMS struct and a random number generator. */
	result = (KNAPSACK_PARAMS *)calloc(1,sizeof(KNAPSACK_PARAMS));
	result->random = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));

	/* Generate a random set of items. Random weights and profits. */
	result->max_profit = 0;
	for (n = 0; n < NUM_ITEMS_TO_CHOOSE; n++) {
		result->profit[n] = RandNextIntRange(result->random,0, ITEM_MAX_VALUE);
		result->weight[n] = RandNextIntRange(result->random,0, ITEM_MAX_WEIGHT);
		result->max_profit+=result->profit[n];
	}

	return result;
}

/* Calculate the total weight for the knapsack */
int CalculateTotalWeight(KNAPSACK_PARAMS *params, unsigned char *items, int item_count) {
	int i;
	double result;

	/* Loop over all items and sum their weights */
	result = 0;
	for (i = 0; i < item_count; i++) {
		if (items[i]) {
			result += params->weight[i];
		}
	}
        
	return (int)result;    
}

/* Balance a knapsack to make sure it is under the max weight.  Do this by randomly 
   removing items from the knapsack until the weight is below.  If we are already below,
   then nothing is removed. */
static void Balance(KNAPSACK_PARAMS *params, unsigned char *items, int item_count) {
	int remove;
	while (CalculateTotalWeight(params,items,item_count) > KNAPSACK_MAX_WEIGHT) {
		remove = RandNextIntRange(params->random, 0, item_count);
		items[remove] = 0;
	}
}

/* This is the randomize function required by Simulated Annealing.  This simply tries to 
   add a random item.  If this puts us over, then remove a random item. */
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
	
	/* We are not holding everything, so try to add something. */
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

/* Score function.  Really, we want to MAXIMIZE the profit.  However, simulated annealing
   seeks to minimize a score.  To do this, we calculate the maximum profit.  Then we
   calculate how far off we are from the maximum profit.  If we are over the weight,
   we set to the lowest score, which is the maximum profit.  Now we have a value to 
   minimize. */
static double score_function(void *m, void *p) {
	TRAIN *train;
	KNAPSACK_PARAMS *params;
	unsigned char *memory;
	unsigned int i;
	double sum = 0;
	
	memory = (unsigned char*)m;
	train = (TRAIN*)p;
	params = (KNAPSACK_PARAMS *)train->params;

	if (CalculateTotalWeight(params,memory,train->position_size) > KNAPSACK_MAX_WEIGHT) {
		return params->max_profit; /* bad socre */
	}

	for (i = 0; i < train->position_size; i++) {
		if (memory[i]) {
			sum += params->profit[i];
		}
	}
	return params->max_profit-sum;
}

/* Fill the knapsack with random items.  This may be over the weight limit, however
   we will rebalance it later. */
void RandomFill(unsigned char *items) {
	int i;
	RANDOM_GENERATE *prng;
	prng = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
	for(i=0;i<NUM_ITEMS_TO_CHOOSE;i++) {
		items[i] = RandNextDouble(prng)>0.5;
	}
}

/*
This example demonstrates a 0-1 knapsack problem, with a potential solution provided by
simulated annealing. The knapsack problem seeks to maximize the value of items held in
a knapsack.  The maximum weight held by the knapsack is limited.

http://en.wikipedia.org/wiki/Knapsack_problem

The output from the knapsack is shown here.  As you can see, the score (or profit) is
slowly maximized.  The final contents are displayed.  The item selection (weight and
profit) are all randomized when the program starts up.

Iteration #1: Score: 6280.000000
Iteration #2: Score: 6280.000000
Iteration #3: Score: 6280.000000
Iteration #4: Score: 6280.000000
Iteration #5: Score: 6279.000000
Iteration #6: Score: 6279.000000
Iteration #7: Score: 6211.000000
Iteration #8: Score: 6048.000000
Iteration #9: Score: 6048.000000
Iteration #10: Score: 5959.000000
Iteration #11: Score: 5889.000000
Iteration #12: Score: 5697.000000
Iteration #13: Score: 5625.000000
Iteration #14: Score: 5595.000000
Iteration #15: Score: 5595.000000
Iteration #16: Score: 5595.000000
Iteration #17: Score: 5457.000000
Iteration #18: Score: 5386.000000
Iteration #19: Score: 5386.000000
Iteration #20: Score: 5386.000000
Iteration #21: Score: 5359.000000
Iteration #22: Score: 5335.000000
Iteration #23: Score: 5334.000000
Iteration #24: Score: 5334.000000
Iteration #25: Score: 5334.000000
Iteration #26: Score: 5334.000000
Iteration #27: Score: 5334.000000
Iteration #28: Score: 5334.000000
Iteration #29: Score: 5334.000000
Iteration #30: Score: 5334.000000
Iteration #31: Score: 5327.000000
Iteration #32: Score: 5327.000000
Iteration #33: Score: 5327.000000
Iteration #34: Score: 5327.000000
Iteration #35: Score: 5327.000000
Iteration #36: Score: 5327.000000
Iteration #37: Score: 5327.000000
Iteration #38: Score: 5327.000000
Iteration #39: Score: 5327.000000
Iteration #40: Score: 5327.000000
Iteration #41: Score: 5327.000000
Iteration #42: Score: 5327.000000
Iteration #43: Score: 5327.000000
Iteration #44: Score: 5327.000000
Iteration #45: Score: 5327.000000
Iteration #46: Score: 5327.000000
Iteration #47: Score: 5327.000000
Iteration #48: Score: 5327.000000
Iteration #49: Score: 5327.000000
Iteration #50: Score: 5327.000000
Iteration #51: Score: 5327.000000
Iteration #52: Score: 5327.000000
Iteration #53: Score: 5327.000000
Iteration #54: Score: 5327.000000
Iteration #55: Score: 5327.000000
Iteration #56: Score: 5327.000000
Iteration #57: Score: 5327.000000
Iteration #58: Score: 5327.000000
Iteration #59: Score: 5327.000000
Iteration #60: Score: 5327.000000
Iteration #61: Score: 5327.000000
Iteration #62: Score: 5327.000000
Iteration #63: Score: 5327.000000
Iteration #64: Score: 5327.000000
Iteration #65: Score: 5327.000000
Iteration #66: Score: 5327.000000
Iteration #67: Score: 5327.000000
Iteration #68: Score: 5327.000000
Iteration #69: Score: 5327.000000
Iteration #70: Score: 5327.000000
Iteration #71: Score: 5327.000000
Iteration #72: Score: 5327.000000
Iteration #73: Score: 5327.000000
Iteration #74: Score: 5327.000000
Iteration #75: Score: 5327.000000
Iteration #76: Score: 5327.000000
Iteration #77: Score: 5327.000000
Iteration #78: Score: 5327.000000
Iteration #79: Score: 5327.000000
Iteration #80: Score: 5327.000000
Iteration #81: Score: 5327.000000
Iteration #82: Score: 5327.000000
Iteration #83: Score: 5327.000000
Iteration #84: Score: 5327.000000
Iteration #85: Score: 5327.000000
Iteration #86: Score: 5327.000000
Iteration #87: Score: 5327.000000
Iteration #88: Score: 5327.000000
Iteration #89: Score: 5327.000000
Iteration #90: Score: 5327.000000
Iteration #91: Score: 5327.000000
Iteration #92: Score: 5327.000000
Iteration #93: Score: 5327.000000
Iteration #94: Score: 5327.000000
Iteration #95: Score: 5327.000000
Iteration #96: Score: 5327.000000
Iteration #97: Score: 5327.000000
Iteration #98: Score: 5327.000000
Iteration #99: Score: 5327.000000
Iteration #100: Score: 5327.000000
Iteration #101: Score: 5327.000000
item	profit	weight
1	149.00	6.00
2	134.00	3.00
17	24.00	0.00
22	120.00	0.00
30	149.00	1.00
31	108.00	2.00
39	146.00	8.00
46	67.00	2.00
49	130.00	2.00
52	44.00	0.00
56	133.00	5.00
58	116.00	5.00
63	102.00	3.00
73	55.00	2.00
74	126.00	3.00
75	50.00	1.00
85	146.00	6.00
88	125.00	0.00
94	109.00	1.00
 
 */
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