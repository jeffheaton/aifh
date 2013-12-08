#include "aifh-vol1-examples.h"


void ExampleLinearRegression(int argIndex, int argc, char **argv) {	
	char filename[FILENAME_MAX];
	NORM_DATA *norm;
	DATA_SET *data;
	REGRESSION_MODEL *reg;
	double *ideal, actual, *input;
	int i;

	LocateFile("abalone.csv",filename,FILENAME_MAX);
	norm = NormCreate();
	NormDefClass(norm,NORM_CLASS_ONEOFN,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);

	NormAnalyze(norm,filename);
	data = NormProcess(norm,filename,8,1);
	
	printf("Actual input count: %i\n", data->inputCount);
	printf("Actual output count: %i\n", data->idealCount);

	reg = RegressionCreate(data->inputCount,LinkLinear);
	RegressionFitLeastSquares(reg,data);

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