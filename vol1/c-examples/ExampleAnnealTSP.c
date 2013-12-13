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

/* The size of the map, i.e. 10x10 */
#define MAP_SIZE 10.0
/* The city count */
#define CITY_COUNT 50
/* The rounded value of PI */
#define pi 3.14159265358979323846

/* The paramaters for the traveling salesman problem. */
typedef struct TSP_PARAMS {
	double city_x[CITY_COUNT];
	double city_y[CITY_COUNT];
	RANDOM_GENERATE *random;
} TSP_PARAMS;

/* Create a TSP problem.  We will arrange the cities in a circle.  Because they are
   in a circle, we know the optimal path. */
static TSP_PARAMS *CreateTSP() {
	TSP_PARAMS *result;
	int i;
	double ratio;

	/* Allocate space to hold the cities. */
	result = (TSP_PARAMS *)calloc(1,sizeof(TSP_PARAMS));
	result->random = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
	ratio = (2 * pi) / CITY_COUNT;

	/* Arrange the cities in a circle. */
	for(i=0;i<CITY_COUNT;i++) {
		result->city_x[i] = (cos(ratio * i) * (MAP_SIZE / 2) + (MAP_SIZE / 2));
		result->city_y[i] = (sin(ratio * i) * (MAP_SIZE / 2) + (MAP_SIZE / 2));
	}

	return result;
}

/* Generate a random path through the cities.  Do not visit a city twice. */
static void random_path(TSP_PARAMS *params, int *path) {
	int x,y,trial_city;

	for(x=0;x<CITY_COUNT;x++) {
		do {
			trial_city = RandNextIntRange(params->random,0,CITY_COUNT);
			for(y=0;y<x;y++) {
				if( path[y]==trial_city ) {
					trial_city = -1;
					break;
				}
			}
			path[x] = trial_city;
		} while(trial_city==-1);
	}
}

/* Score the specified path.  The score is the Euclidean distance between all segments
   of the path. */
static double score_function(void *m, void *p) {
	TRAIN *train;
	TSP_PARAMS *params;
	int *memory,i;
	double result = 0;
	
	memory = (int*)m;
	train = (TRAIN*)p;
	params = (TSP_PARAMS *)train->params;

    /* Cities. */
	for(i=0;i<(CITY_COUNT-1);i++) {
		result+=sqrt( pow(params->city_x[memory[i]]-params->city_x[memory[i+1]],2)+
			pow(params->city_y[memory[i]]-params->city_y[memory[i+1]],2) );
	}

	return result;
}

/* The randomize function required by Simulated Annealing. 
   To randomize we simply pick two cities at random and swap.
   This cannot violate the requirement of not visiting the same city twice,
   so long as the input does not violate this requirement. 
*/
static void tsp_randomize(void *a) {
	TRAIN_ANNEAL *anneal;
	int dimensions, d, *position, city1,city2;

	anneal = (TRAIN_ANNEAL*)a;
	position = (int*)anneal->train.trial_position;
	dimensions = anneal->train.position_size/sizeof(int);
	
	do {
		city1 = RandNextIntRange(anneal->train.random,0,CITY_COUNT);
		city2 = RandNextIntRange(anneal->train.random,0,CITY_COUNT);
	} while(city1==city2);

	d = position[city2];
	position[city2] = position[city1];
	position[city1] = d;
	
}

/* 
This example uses discrete simulated annealing with the Traveling Salesman Problem (TSP).  
The cities are placed in a circle, so the ideal path is known.  Because the cities are in 
a circle they should be visited in order around the edge for the absolute optimal path.
http://en.wikipedia.org/wiki/Traveling_salesman_problem

Running the program produces the following output.  Simulated annealing always performs 
the maximum number of iterations, unless stopped early.  Here we do not specify a stopping 
score, so the full 500 iterations are used. Each line of output shows the iteration number, 
the score, the k (also the iteration number), kmax (the max number of iterations), t (the 
temperature), and prob (the probability of accepting a worse solution than the current).  
You can see that an optimal solution was not found, but we are close!  There are large 
ranges of numbers in order.  We stay close to the edge of the circle.


Iteration #1: Score: 253.988673
Iteration #2: Score: 253.988673
Iteration #3: Score: 253.988673
Iteration #4: Score: 253.988673
Iteration #5: Score: 253.988673
Iteration #6: Score: 253.988673
Iteration #7: Score: 253.988673
Iteration #8: Score: 251.876943
Iteration #9: Score: 251.876943
Iteration #10: Score: 251.876943
Iteration #11: Score: 251.876943
Iteration #12: Score: 251.876943
Iteration #13: Score: 247.853528
Iteration #14: Score: 247.853528
Iteration #15: Score: 247.853528
Iteration #16: Score: 247.853528
Iteration #17: Score: 247.853528
Iteration #18: Score: 247.853528
Iteration #19: Score: 247.853528
Iteration #20: Score: 247.853528
Iteration #21: Score: 247.853528
Iteration #22: Score: 247.853528
Iteration #23: Score: 247.853528
Iteration #24: Score: 247.853528
Iteration #25: Score: 247.853528
Iteration #26: Score: 247.853528
Iteration #27: Score: 235.651938
Iteration #28: Score: 235.651938
...
Iteration #1992: Score: 34.956986
Iteration #1993: Score: 34.956986
Iteration #1994: Score: 34.956986
Iteration #1995: Score: 34.956986
Iteration #1996: Score: 34.956986
Iteration #1997: Score: 34.956986
Iteration #1998: Score: 34.956986
Iteration #1999: Score: 34.956986
Iteration #2000: Score: 34.956986
Iteration #2001: Score: 34.956986
Final Path:
1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 0 -> 49 -> 48 -> 47 -> 46 -> 45 -> 44 -> 43 -> 
42 -> 41 -> 40 -> 39 -> 38 -> 37 -> 36 -> 35 -> 34 -> 33 -> 32 -> 31 -> 30 -> 29 -> 28 -> 
27 -> 26 -> 25 -> 24 -> 23 -> 22 -> 21 -> 20 -> 19 -> 18 -> 17 -> 16 -> 15 -> 14 -> 13 -> 
12 -> 11 -> 10 -> 9 -> 
*/
void ExampleAnnealTSP(int argIndex, int argc, char **argv) {
	TSP_PARAMS *params;
	TRAIN *train;
	int x0[CITY_COUNT], *x;
	unsigned int size,i;
	const int kMax = 2000;

	params = CreateTSP();
	random_path(params, x0);
	size = CITY_COUNT * sizeof(int);
	
	train = TrainCreateAnneal(score_function,x0,size,400,0.0001,500,kMax,params);
	((TRAIN_ANNEAL*)train)->anneal_randomize = tsp_randomize;
	TrainRun(train,kMax,0.01,1);

	/* Print out best path. */
	x = (int*)train->best_position;

	printf("Final Path:\n");
	for(i=0;i<CITY_COUNT;i++) {
		printf("%i -> ",x[i]);
	}
	printf("\n");
	
	free(params);
}