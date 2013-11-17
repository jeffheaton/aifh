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
	items = NULL;
	NormConcatClassItem(&items,"red");
	NormConcatClassItem(&items,"green");
	NormConcatClassItem(&items,"blue");
	NormConcatClassItem(&items,"yellow");
	NormConcatClassItem(&items,"purple");
	NormOneOfN(items,TARGET_LOW,TARGET_HIGH,"yellow",dataVec);
	DisplayDoubleVector(dataVec,5);
}