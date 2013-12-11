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

void NormConcatClassItem(NORM_DATA_CLASS **items, char *className) {
	NORM_DATA_CLASS *lastClass;
	NORM_DATA_CLASS *newClass;
	int done;

	/* First scan the list and perform two functions */
	/* Function 1: Does this class already exist? */
	/* Function 2: Find the last item so that we can append later if the item does not exist */
	lastClass = *items;
	done=0;
	while(!done && lastClass!=NULL) {
		/* exit if we already have one */
		if(!strcmp(lastClass->name,className)) {
			return;
		}
		if( lastClass->next == NULL ) {
			done = 1;
		} else {
			lastClass=lastClass->next;
		}
	}

	/* Create a new class item */
	newClass = (NORM_DATA_CLASS*)calloc(1,sizeof(NORM_DATA_CLASS));
	newClass->name = strdup(className);

	/* Now add it */
	if( lastClass==NULL ) {
		/* New first item */
		*items = newClass;
	} else {
		lastClass->next = newClass;
	}
}

void DisplayDoubleVector(double *vec, int size) {
	int i;
	printf("[");
	for(i=0;i<size;i++) {
		if( i!=0 ) {
			printf(",");
		}
		printf("%.4f",vec[i]);
	}
	printf("]");
}

void ExampleSimpleNormalize(int argIndex, int argc, char **argv) {	
	const double ACTUAL_LOW = 0;
	const double ACTUAL_HIGH = 10;
	const double TARGET_LOW = 0;
	const double TARGET_HIGH = 1;
	double x,y;
	NORM_DATA_CLASS *items;
	double dataVec[5];
	char *s;
	double matrix[20];
	int i;

	printf("***Range***\n");
	/* Range normalize */
	x = NormRange(ACTUAL_LOW,ACTUAL_HIGH,TARGET_LOW,TARGET_HIGH,5);
	printf("5 range normalized is %f\n",x);
	/* Range denormalize */
	y = DeNormRange(ACTUAL_LOW,ACTUAL_HIGH,TARGET_LOW,TARGET_HIGH,x);
	printf("%f range denormalized is %f\n",x,y);

	printf("***Reciprocal***\n");
	/* Reciprocal normalize */
	x = NormReciprocal(5);
	printf("5 reciprocal normalized is %f\n",x);
	/* Reciprocal denormalize */
	y = DeNormReciprocal(x);
	printf("%f reciprocal denormalized is %f\n",x,y);

	printf("***One-of-N***\n");
	/* One-of-N */
	items = NULL;
	NormConcatClassItem(&items,"red");
	NormConcatClassItem(&items,"green");
	NormConcatClassItem(&items,"blue");
	NormConcatClassItem(&items,"yellow");
	NormConcatClassItem(&items,"purple");
	NormOneOfN(items,TARGET_LOW,TARGET_HIGH,"yellow",dataVec);
	printf("yellow normalizes one-of-n to ");
	DisplayDoubleVector(dataVec,5);
	printf("\n");
	DisplayDoubleVector(dataVec,5);
	s = DeNormOneOfN(items,TARGET_LOW,TARGET_HIGH,dataVec);
	printf(" denormalizes one-of-n to %s\n",s);

	/* Construct Equilateral */
	printf("***Equalateral Matrix***\n");
	Equilat(5,-1,1,matrix);
	for(i=0;i<5;i++) {
		printf("%i:",i);
		DisplayDoubleVector(matrix+(i*4),4);
		printf("\n");
	}


	printf("***Equalateral***\n");
	/* Equilateral */
	NormEquilateral(items,matrix,TARGET_LOW,TARGET_HIGH,"yellow",dataVec);
	printf("yellow equilateral one-of-n to ");
	DisplayDoubleVector(dataVec,4);
	printf("\n");
	DisplayDoubleVector(dataVec,5);
	s = DeNormEquilateral(items,matrix,5, TARGET_LOW,TARGET_HIGH,dataVec);
	printf(" denormalizes equilateral to %s\n",s);
}