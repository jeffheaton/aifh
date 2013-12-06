#include "aifh-vol1.h"

double RBFGaussian(double *input, int input_position, int input_count, double *params, int params_index) {
	double value = 0, center;
	double width = params[params_index];
	int i;

	for (i = 0; i < input_count; i++) {
		center = params[1+params_index+i];
		value += pow(input[input_position+i] - center, 2) / (2.0 * width * width);
	}
	return exp(-value);
}