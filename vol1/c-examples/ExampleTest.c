/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * C/C++ Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
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