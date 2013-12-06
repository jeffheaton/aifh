#include "aifh-vol1-examples.h"


void ExampleTest(int argIndex, int argc, char **argv) {	
	RBF_NETWORK *net;
	double input[2] = { 1.0, 2.0 };
	double output;

	net = RBFNetworkCreate(RBFGaussian,2,1,1);
	net->long_term_memory[0] = 2.0;
	net->long_term_memory[1] = 2.0;
	net->long_term_memory[2] = 5.0;
	net->long_term_memory[3] = 2.0;
	net->long_term_memory[4] = 4.0;
	net->long_term_memory[5] = 3.0;
	net->long_term_memory[6] = 4.0;

	RBFNetworkComputeRegression(net,input,&output);
	printf("%f\n",output);

	printf("done");
}