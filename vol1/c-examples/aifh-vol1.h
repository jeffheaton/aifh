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

#include "csv.h"

#define NORM_TYPE_RANGE 0
#define NORM_CLASS_ONEOFN 1
#define NORM_CLASS_EQUILATERAL 2

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
#else
/* For non-Visual C++ */
#include <unistd.h>
#endif

	
/* Distance.c */

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

NORM_DATA *NormCreate();
void NormDelete(NORM_DATA *norm);
void NormDefRange(NORM_DATA *norm, double low, double high);
void NormDefClass(NORM_DATA *norm, int type, double low, double high);
void NormAnalyze(NORM_DATA *data, char *filename);
DATA_SET *NormProcess(NORM_DATA *norm, char *filename, int inputCount, int outputCount);
int CalculateActualCount(NORM_DATA *norm,int start, int size);

#ifdef __cplusplus
}
#endif

#endif