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
	unsigned int i;
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
	unsigned int rowCount, inputColCount, row, col, colSize, i;
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


double RegressionReweightLeastSquares(REGRESSION_MODEL *reg, DATA_SET *training) {
	unsigned int rowCount, coeffCount, i, j, k;
    mat working, deltas, hessian, gradient;
    double *errors, *weights,*inputData, *idealData, *r, *prev, max, y, d;

	rowCount = training->recordCount;
	coeffCount = reg->count;

	working = matrix_new(rowCount,coeffCount);
    errors = (double*)calloc(rowCount,sizeof(double));
	weights = (double*)calloc(rowCount,sizeof(double));
	hessian = matrix_new(coeffCount,coeffCount);
	gradient = matrix_new(coeffCount,1);
	prev = (double*)calloc(coeffCount,sizeof(double));

	for (i = 0; i < rowCount; i++) {
		inputData = DataGetInput(training,i);

		working->v[i][0] = 1;
			
		for (j = 0; j < training->inputCount; j++)
                working->v[i][j + 1] = inputData[j];
        }

        for (i = 0; i < rowCount; i++) {
			inputData = DataGetInput(training,i);
			idealData = DataGetIdeal(training,i);

			y = RegressionCalculate(reg,inputData);
            errors[i] = y - *idealData;
            weights[i] = y * (1.0 - y);
        }

		for (i = 0; i < gradient->n; i++) {
            gradient->v[0][i] =  0;
            for (j = 0; j < gradient->n; j++)
                hessian->v[i][j] = 0;
        }

		for (j = 0; j < working->m; j++) {
            for (i = 0; i < gradient->m; i++) {
                gradient->v[i][0] = gradient->v[i][0] + working->v[j][i] * errors[j];
            }
        }

		for (k = 0; k < rowCount; k++) {
			r = working->v[k];
            
			for (j = 0; j < coeffCount; j++) {
				for (i = 0; i < coeffCount; i++) {
                    hessian->v[j][i] += r[i] * r[j] * weights[k];
                }
            }
        }

		deltas = matrix_solve_lu(hessian,gradient);
		memcpy(prev,reg->coeff,sizeof(double)*coeffCount);
        
		for (i = 0; i < coeffCount; i++) {
			reg->coeff[i] -= deltas->v[i][0];
		}

        max = 0;
		for (i = 0; i < deltas->n; i++) {
			d = (fabs(deltas->v[i][0]) / fabs(prev[i]));
            max = MAX(d,max);// MAX(fabs(deltas.get(i, 0)) / fabs(prev[i]), max);
		}

		/* Cleanup */
		matrix_delete(working);
		matrix_delete(deltas);
		matrix_delete(hessian);
		matrix_delete(gradient);

		free(errors);
		free(weights);
		free(prev);
        return max;
}
