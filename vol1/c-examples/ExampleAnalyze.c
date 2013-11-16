#include "aifh-vol1-examples.h"

void ExampleAnalyze(int argIndex, int argc, char **argv) {	
	char filename[FILENAME_MAX];
	NORM_DATA *norm;
	NORM_DATA_ITEM *col;
	NORM_DATA_CLASS *currentClass;

	LocateFile("iris.csv",filename,FILENAME_MAX);
	norm = NormCreate();
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefClass(norm,NORM_CLASS_ONEOFN,0,1);
	NormAnalyze(norm,filename);
	
	col = norm->firstItem;
	while(col!=NULL) {

		if( col->type == NORM_TYPE_RANGE ) {
			printf("Column: \"%s\",actualMin=%.2f,actualHigh=%.2f\n",col->name,col->actualHigh,col->actualLow);
		} else {
			printf("Column: \"%s\",classes= ",col->name);
			currentClass = col->firstClass;
			while(currentClass!=NULL) {
				printf("\"%s\";",currentClass->name);
				currentClass = currentClass->next;
			}
			printf("\n");

		}
		col = col->next;
	}

	printf("Rows: %i\n",norm->rowCount);

	NormDelete(norm);
}