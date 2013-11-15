#include "aifh-vol1-examples.h"


void ExampleNormalize(int argIndex, int argc, char **argv) {	
	char filename[FILENAME_MAX];
	NORM_DATA *norm;
	NORM_DATA_ITEM *col;

	LocateFile("iris.csv",filename,FILENAME_MAX);
	norm = NormCreate();
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefClass(norm,0,1,NORM_CLASS_ONEOFN);
	NormProcess(norm,filename,4,1);
	
	col = norm->firstItem;
	while(col!=NULL) {
		printf("Column: \"%s\",actualMin=%.2f,actualHigh=%.2f\n",col->name,col->actualHigh,col->actualLow);
		col = col->next;
	}

	printf("Rows: %i\n",norm->rowCount);
	NormDelete(norm);
}