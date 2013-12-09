#include "aifh-vol1.h"

double LinkLOGIT(double x) {
	return 1.0 / (1.0 + exp(-x));
}

double LinkLinear(double x) {
	return x;
}

REGRESSION_MODEL *RegressionCreate(int inputCount, LINK_FUNCTION link) {
	REGRESSION_MODEL *result;

	result = (REGRESSION_MODEL *)calloc(1,sizeof(REGRESSION_MODEL));
	result->count = inputCount+1;
	result->coeff = (double*)calloc(result->count,sizeof(double));
	result->link = link;
	return result;
}

void RegressionDelete(REGRESSION_MODEL *reg) {
	free(reg->coeff);
	free(reg);
}

double RegressionCalculate(REGRESSION_MODEL *reg, double *x) {
	double sum = 0;
	int i;
	double d1,d2,d3;

	for (i = 1; i < reg->count; i++) {
		d1 = x[i-1];
		d2 = reg->coeff[i];
		d3 = d1*d2;
		sum += x[i - 1] * reg->coeff[i];
	}
        
	sum += reg->coeff[0];
	return reg->link(sum);
}

void RegressionFitLeastSquares(REGRESSION_MODEL *reg, DATA_SET *training) {
	int rowCount, inputColCount, row, col, colSize, i;
	mat xMatrix, yMatrix;
	double *inputData, *idealData;
	mat beta;

	if( training->idealCount!=1 ) {
		printf("Ideal count may only be 1 for least squares fitting.\n");
		exit(1);
	}

	if( training->inputCount != (reg->count-1) ) {
		printf("Coefficient count of %i requires a training input count of %i, training input count is %i.\n",
			reg->count, reg->count+1, training->inputCount);
		exit(1);
	}

	rowCount = training->recordCount;
	inputColCount = training->inputCount;

	xMatrix = matrix_new(rowCount, inputColCount + 1);
    yMatrix = matrix_new(rowCount, 1);

	colSize = training->inputCount;
	for (row = 0; row < training->recordCount; row++) {
		inputData = DataGetInput(training,row);
		idealData = DataGetIdeal(training,row);
		xMatrix->v[row][0] = 1.0;
		for (col = 0; col < colSize; col++) {
			xMatrix->v[row][col+1] = inputData[col];
            }
		yMatrix->v[row][0] = idealData[0];
	}

	beta = matrix_solve_qr(xMatrix,yMatrix);

	for(i=0;i<reg->count;i++) {
		reg->coeff[i] = beta->v[i][0];
	}
}
