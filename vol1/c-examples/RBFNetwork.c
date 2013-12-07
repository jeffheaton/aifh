#include "aifh-vol1.h"


RBF_NETWORK* RBFNetworkCreate(RBF_FUNCTION rbf, int input_count, int rbf_count, int output_count) {
	RBF_NETWORK *result;
	int i;

	/* calculate input and output weight counts
	   add 1 to output to account for an extra bias node */
	const int inputWeightCount = input_count * rbf_count;
	const int outputWeightCount = (rbf_count + 1) * output_count;
	const int rbfParams = (input_count + 1) * rbf_count;

	/* allocate structure */
	result = (RBF_NETWORK*)calloc(1,sizeof(RBF_NETWORK));
	result->input_count = input_count;
	result->rbf_count = rbf_count;
	result->rbf = rbf;
	result->output_count = output_count;
	result->rbf_index = (unsigned int*)calloc(rbf_count,sizeof(int));
	result->rbf_output = (double*)calloc(rbf_count+1,sizeof(double));
	result->weighted_input = (double*)calloc(input_count,sizeof(double));

	result->ltm_size = inputWeightCount + outputWeightCount + rbfParams;
	result->long_term_memory = (double*)calloc(result->ltm_size,sizeof(double));

	result->index_input_weights = 0;
	result->index_output_weights = inputWeightCount + rbfParams;

	for (i = 0; i < rbf_count; i++) {
		result->rbf_index[i] = inputWeightCount + ((input_count + 1) * i);
	}

	return result;
}

void RBFNetworkDelete(RBF_NETWORK *network) {
	free(network->rbf_index);
	free(network->rbf_output);
	free(network->weighted_input);
	free(network);
}

void RBFNetworkReset(RBF_NETWORK *network) {
	unsigned int i;
	RANDOM_GENERATE *prng;

	prng = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
	for(i=0;i<network->ltm_size;i++) {
		network->long_term_memory[i] = RandNextDoubleRange(prng,-1,1);
	}

	RandDelete(prng);
}


void RBFNetworkComputeRegression(RBF_NETWORK *network, double *input, double *output) {
	unsigned int inputIndex, outputIndex, rbfIndex, memoryIndex;
	double sum;

	/* first, compute the output values of each of the RBFs
	   Add in one additional RBF output for bias (always set to one). */

	network->rbf_output[network->rbf_count] = 1; // bias

	for (rbfIndex = 0; rbfIndex < network->rbf_count; rbfIndex++) {
		/* weight the input */
		for (inputIndex = 0; inputIndex < network->input_count; inputIndex++) {
			memoryIndex = network->index_input_weights + (rbfIndex * network->input_count) + inputIndex;
			network->weighted_input[inputIndex] = input[inputIndex] * network->long_term_memory[memoryIndex];
		}

		/* calculate the rbf */
		network->rbf_output[rbfIndex] = network->rbf(network->weighted_input,0,network->input_count,network->long_term_memory,network->rbf_index[rbfIndex]);
	}

	/* second, calculate the output, which is the result of the weighted result of the RBF's. */

	for (outputIndex = 0; outputIndex < network->output_count; outputIndex++) {
		sum = 0;
		for (rbfIndex = 0; rbfIndex <= network->rbf_count; rbfIndex++) {
			/* add 1 to rbf length for bias */
			memoryIndex = network->index_output_weights + (outputIndex * (network->rbf_count + 1)) + rbfIndex;
			sum += network->rbf_output[rbfIndex] * network->long_term_memory[memoryIndex];
		}
		output[outputIndex] = sum;
	}
}