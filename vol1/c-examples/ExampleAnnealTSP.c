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

#define MAP_SIZE 10.0
#define CITY_COUNT 50
#define pi 3.14159265358979323846

typedef struct IRIS_PARAMS {
	double city_x[CITY_COUNT];
	double city_y[CITY_COUNT];
	RANDOM_GENERATE *random;
} TSP_PARAMS;

static TSP_PARAMS *CreateTSP() {
	TSP_PARAMS *result;
	int i;
	double ratio;

	result = (TSP_PARAMS *)calloc(1,sizeof(TSP_PARAMS));
	result->random = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
	ratio = (2 * pi) / CITY_COUNT;

	for(i=0;i<CITY_COUNT;i++) {
		result->city_x[i] = (cos(ratio * i) * (MAP_SIZE / 2) + (MAP_SIZE / 2));
		result->city_y[i] = (sin(ratio * i) * (MAP_SIZE / 2) + (MAP_SIZE / 2));
	}

	return result;
}

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

static double score_function(void *m, void *p) {
	TRAIN *train;
	TSP_PARAMS *params;
	int *memory,i;
	double result = 0;
	
	memory = (int*)m;
	train = (TRAIN*)p;
	params = (TSP_PARAMS *)train->params;

	for(i=0;i<(CITY_COUNT-1);i++) {
		result+=sqrt( pow(params->city_x[memory[i]]-params->city_x[memory[i+1]],2)+
			pow(params->city_y[memory[i]]-params->city_y[memory[i+1]],2) );
	}

	return result;
}

static void tsp_randomize(void *a) {
	TRAIN_ANNEAL *anneal;
	int dimensions,i;
	int d, *position, city1,city2;

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
void ExampleAnnealTSP(int argIndex, int argc, char **argv) {
	TSP_PARAMS *params;
	TRAIN *train;
	int x0[CITY_COUNT], *x;
	unsigned int size,i;
	char *idealSpecies,*actualSpecies;
	const int kMax = 2000;

	params = CreateTSP();
	random_path(params, x0);
	size = CITY_COUNT * sizeof(int);
	
	train = TrainCreateAnneal(score_function,x0,size,400,0.0001,500,kMax,params);
	((TRAIN_ANNEAL*)train)->anneal_randomize = tsp_randomize;
	TrainRun(train,kMax,0.01,1);

	/* Print out best path. */
	x = (int*)train->best_position;

	for(i=0;i<CITY_COUNT;i++) {
		printf("%i -> ",x[i]);
	}
	printf("\n");
	
	free(params);
}