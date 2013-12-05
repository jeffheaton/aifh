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
#ifndef __AIFH_VOL1_H
#define __AIFH_VOL1_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>	
#include <errno.h>
#include <math.h>
#include <time.h>
#include <omp.h>
#include <stdint.h>
 
#include "csv.h"

#define NORM_TYPE_RANGE 0
#define NORM_TYPE_RECIPROCAL 1
#define NORM_CLASS_ONEOFN 2
#define NORM_CLASS_EQUILATERAL 3


#define TYPE_RANDOM_C 0
#define TYPE_RANDOM_LCG 1
#define TYPE_RANDOM_MWC 2
#define TYPE_RANDOM_MT 3

#define TYPE_ERROR_SSE 0
#define TYPE_ERROR_MSE 1
#define TYPE_ERROR_RMS 2

#define TYPE_TRAIN_GREEDY_RANDOM 0
#define TYPE_TRAIN_HILL_CLIMB 1
#define TYPE_TRAIN_ANNEAL 2
#define TYPE_TRAIN_NELDER_MEAD 3

#define MAX_STR 256

#ifndef MAX
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

/* For visual C++ */
#ifdef _MSC_VER
#include <io.h>
#define strcasecmp _strcmpi
#pragma warning(disable : 4996)
#define F_OK    0       /* Test for existence.  */
#elif __APPLE__
#include <strings.h>
#else
/* For non-Visual C++ */
#include <unistd.h>
#endif

typedef double(*SCORE_FUNCTION)(void *position, void *params);
typedef double(*RBF_FUNCTION)(double *input, int input_position, int input_count, double *params, int params_index);


	
/* Distance.c */
typedef double(*DISTANCE_FUNCTION)(double *position1, 
	int pos1, 
	double *position2, 
	int pos2, 
	int length);

double DistanceEuclidean(
	double *position1, 
	int pos1, 
	double *position2, 
	int pos2, 
	int length);

double DistanceManhattan(
	double *position1, 
	int pos1, 
	double *position2, 
	int pos2, 
	int length);

double DistanceChebyshev(
	double *position1, 
	int pos1, 
	double *position2, 
	int pos2, 
	int length);


typedef struct NORM_DATA_CLASS {
	char *name;
	struct NORM_DATA_CLASS *next;
} NORM_DATA_CLASS;

typedef struct NORM_DATA_ITEM {
	double actualHigh;
	double actualLow;
	double targetHigh;
	double targetLow;
	char *name;
	int type;
	int classCount;
	double *equilateral;
	struct NORM_DATA_ITEM *next;
	NORM_DATA_CLASS *firstClass;
} NORM_DATA_ITEM;

typedef struct NORM_DATA {
	NORM_DATA_ITEM *firstItem;
	int columnCount;
	int rowCount;
	int _currentColumn;
} NORM_DATA;

typedef struct DATA_SET
{
    int inputCount;
    int idealCount;
    unsigned long recordCount;
	double *cursor;
    double *data;
	NORM_DATA_CLASS * firstClass;
} DATA_SET;

typedef struct RANDOM_GENERATE {
	int type;
	unsigned int seed;
	int useLast;
	double y2;
} RANDOM_GENERATE;

typedef struct RANDOM_GENERATE_LCG {
	RANDOM_GENERATE base;
    unsigned int modulus;
    unsigned int multiplier;
    unsigned int increment;
} RANDOM_GENERATE_LCG;

typedef struct RANDOM_GENERATE_MWC {
	RANDOM_GENERATE base;
    uint32_t Q[4096]; 
	uint32_t c;
	int idx;
} RANDOM_GENERATE_MWC;

typedef struct CLUSTER_ITEM {
	double *features;
	char *label;
	struct CLUSTER_ITEM *next;
	double data;
} CLUSTER_ITEM;

typedef struct CLUSTER {
	double *centroid;
	struct CLUSTER_ITEM *firstItem;
} CLUSTER;

typedef struct CLUSTER_ALOG {
	int k;
	int featureCount;
	RANDOM_GENERATE *rnd;
	DISTANCE_FUNCTION dist;
	CLUSTER *clusters;
} CLUSTER_ALOG;

typedef struct ERROR_CALC {
	int type;
	int count;
	double global_error;
} ERROR_CALC;

typedef struct RBF_NETWORK {
	RBF_FUNCTION rbf;
	int ltm_size;
    int input_count;
	int rbf_count;
    int output_count;
    double *long_term_memory;
	double *rbf_output;
	double *weighted_input;
	int *rbf_index;
    int index_input_weights;
    int index_output_weights;
} RBF_NETWORK;

typedef struct TRAIN {
	int type;
	double low;
	double high;
	int position_size;
	unsigned char *best_position;
	unsigned char *current_position;
	unsigned char *trial_position;
	double best_score;
	int should_minimize;
	void *params;
	SCORE_FUNCTION score_function;
	RANDOM_GENERATE *random;
	int max_iterations;
} TRAIN;

NORM_DATA *NormCreate();
void NormDelete(NORM_DATA *norm);
void NormDefRange(NORM_DATA *norm, double low, double high);
void NormDefClass(NORM_DATA *norm, int type, double low, double high);
void NormAnalyze(NORM_DATA *data, char *filename);
DATA_SET *NormProcess(NORM_DATA *norm, char *filename, int inputCount, int outputCount);
int NormCalculateActualCount(NORM_DATA *norm,int start, int size);
double NormRange(double dataLow, double dataHigh, double normalizedLow, double normalizedHigh, double x);
void NormOneOfN(NORM_DATA_CLASS *first, double normalizedLow, double normalizedHigh, char *classX, double *dataOut);
double DeNormRange(double dataLow, double dataHigh, double normalizedLow, double normalizedHigh, double x);
double NormReciprocal(double x);
double DeNormReciprocal(double x);
char* DeNormOneOfN(NORM_DATA_CLASS *first, double normalizedLow, double normalizedHigh, double *dataOut);
void NormEquilateral(NORM_DATA_CLASS *first, double *equilat, double normalizedLow, double normalizedHigh, char *classX, double *dataOut);
char* DeNormEquilateral(NORM_DATA_CLASS *first, double *equilat, int classCount, double normalizedLow, double normalizedHigh, double *dataOut);

/* Data.c */
DATA_SET *DataCreate(int rowCount, int inputCount, int outputCount);
void DataDelete(DATA_SET *data);
double *DataGetInput(DATA_SET *data, unsigned int index);
double *DataGetIdeal(DATA_SET *data, unsigned int index);
void DataCSVSave(FILE *fp,NORM_DATA *norm, DATA_SET *data);

/* Equilateral.c */
void Equilat (
   int classCount,
   double low,
   double high,
   double *outputMatrix
   );

/* Error.c */
ERROR_CALC *ErrorCreate(int type);
void ErrorDelete(ERROR_CALC *calc);
void ErrorReset(ERROR_CALC *);
void ErrorUpdateSingle(ERROR_CALC *, double d1, double d2);
void ErrorUpdate(ERROR_CALC *, double *d1, double *d2, size_t size);
double ErrorCalculate(ERROR_CALC*);

/* Random.c */
RANDOM_GENERATE *RandCreate(int type, long seed);
void RandDelete(RANDOM_GENERATE *gen);
long RandNextInt(RANDOM_GENERATE *gen);
double RandNextDouble(RANDOM_GENERATE *gen);
double RandNextGaussian(RANDOM_GENERATE *gen);
int RandNextIntRange(RANDOM_GENERATE *gen, int low, int high);
double RandNextDoubleRange(RANDOM_GENERATE *gen, double low, double high);

/* mt19937ar.c */
void init_genrand(unsigned long s);
void init_by_array(unsigned long init_key[], int key_length);
unsigned long genrand_int32(void);
long genrand_int31(void);
double genrand_real1(void);
double genrand_real2(void);
double genrand_real3(void);
double genrand_res53(void);


/* KMeans.c */
CLUSTER_ITEM *CreateClusterItem(int featureCount, char *label);
void DeleteClusterItem(CLUSTER_ITEM *item);
void DeleteKMeansList(CLUSTER_ITEM *first);
void DeleteKMeansItem(CLUSTER_ITEM *first);
void DeleteKMeans(CLUSTER_ALOG *alog);
CLUSTER_ALOG *CreateKMeans(int k,int featureCount);
int KMeansCountItems(CLUSTER_ITEM *first);
CLUSTER_ITEM *KMeansFindItem(CLUSTER_ITEM *first, int index);
void KMeansRemoveItem(CLUSTER_ITEM **first, CLUSTER_ITEM *targetItem);
void KMeansInitRandom(CLUSTER_ALOG *kmeans, CLUSTER_ITEM *items);
CLUSTER *KMeansFindNearestCluster(CLUSTER_ALOG *kmeans, CLUSTER_ITEM *item);
void KMeansInitForgy(CLUSTER_ALOG *kmeans, CLUSTER_ITEM *items);
void KMeansUpdateStep(CLUSTER_ALOG *kmeans);
int KMeansAssignStep(CLUSTER_ALOG *kmeans);
int KMeansIteration(CLUSTER_ALOG *kmeans);
CLUSTER_ITEM* KMeansLoadCSV(char *filename, int labelColumn, int startColumn, int featureCount);
void KMeansDumpList(FILE *out, CLUSTER_ITEM *first, int featureCount);
void KMeansDump(FILE *out, CLUSTER_ALOG *alog);

/* RBFNetwork.c */
RBF_NETWORK* RBFNetworkCreate(RBF_FUNCTION rbf, int input_count, int rbf_count, int output_count);
void RBFNetworkDelete(RBF_NETWORK *network);

/* RBF.c */

#ifdef __cplusplus
}
#endif

#endif