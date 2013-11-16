#include "aifh-vol1-examples.h"


void ExampleNormalize(int argIndex, int argc, char **argv) {	
	char filename[FILENAME_MAX];
	NORM_DATA *norm;
	DATA_SET *data;
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
	data = NormProcess(norm,filename,4,1);
	
	printf("Actual input count: %i\n", data->inputCount);
	printf("Actual output count: %i\n", data->idealCount);

	DataCSVSave(stdout,norm,data);
	NormDelete(norm);
}