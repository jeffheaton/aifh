#include "aifh-vol1-examples.h"


void ExampleNormalize(int argIndex, int argc, char **argv) {	
	char filename[FILENAME_MAX];

	NORM_DATA *norm = NormCreate();
	NormDefRange(norm,0,1);
	NormProcess(norm,filename,4,1);
	NormDelete(norm);
}