#include "aifh-vol1-examples.h"


void ExampleGLM(int argIndex, int argc, char **argv) {	
	char filename[FILENAME_MAX];
	NORM_DATA *norm;
	DATA_SET *data;
	REGRESSION_MODEL *reg;
	double *ideal, actual, *input;
	unsigned int i;

	LocateFile("breast-cancer-wisconsin.csv",filename,FILENAME_MAX);
	norm = NormCreate();
	NormDefIgnore(norm);
	NormDefPass(norm);
	NormDefPass(norm);
	NormDefPass(norm);
	NormDefPass(norm);
	NormDefPass(norm);
	NormDefPass(norm);
	NormDefPass(norm);
	NormDefPass(norm);
	NormDefPass(norm);
	NormDefReplace(norm,4,1,0);

	NormAnalyze(norm,filename);
	data = NormProcess(norm,filename,9,1);
	
	printf("Actual input count: %i\n", data->inputCount);
	printf("Actual output count: %i\n", data->idealCount);

	reg = RegressionCreate(data->inputCount,LinkLOGIT);
	RegressionReweightLeastSquares(reg,data);

	for(i=0;i<data->recordCount;i++) {
		ideal = DataGetIdeal(data,i);
		input = DataGetInput(data,i);
		actual = RegressionCalculate(reg,input);
		printf("Ideal: %.2f, Actual: %.2f\n",ideal[0],actual);
	}

	NormDelete(norm);
	DataDelete(data);
	RegressionDelete(reg);
}