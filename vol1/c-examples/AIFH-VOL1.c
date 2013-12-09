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

int main(int argc, char* argv[])
{
	int i;
	int pause = 0;
	int success = 0;

	for(i=1;i<argc;i++) {
		if( *argv[i]=='-' ) {
			if( strcasecmp(argv[i],"-pause") == 0) {
				pause = 1;
			} else {
				printf("Unknown option: %s",argv[i]);
			}
		}
		else {
			if( strcasecmp(argv[i],"CSVExample") == 0 ) {
				success = 1;
				ExampleReadCSV(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"NormalizeCSVExample") == 0 ) {
				success = 1;
				ExampleNormalize(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"SimpleNormalize") == 0 ) {
				success = 1;
				ExampleSimpleNormalize(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"Analyze") == 0 ) {
				success = 1;
				ExampleAnalyze(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"Distance") == 0 ) {
				success = 1;
				ExampleDistance(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"random") == 0 ) {
				success = 1;
				ExampleRandom(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"EvaluatePI") == 0 ) {
				success = 1;
				ExamplePI(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"PerformCluster") == 0 ) {
				success = 1;
				ExampleKMeans(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"EvaluateErrors") == 0 ) {
				success = 1;
				ExampleError(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"LearnPolynomial") == 0 ) {
				success = 1;
				ExamplePoly(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"LearnXOR") == 0 ) {
				success = 1;
				ExampleRandXOR(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"LearnIris") == 0 ) {
				success = 1;
				ExampleRandIris(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"LearnIrisNelderMead") == 0 ) {
				success = 1;
				ExampleNelderMeadIris(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"LearnIrisClimb") == 0 ) {
				success = 1;
				ExampleHillClimbIris(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"LearnIrisAnneal") == 0 ) {
				success = 1;
				ExampleAnnealIris(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"TravelingSalesmanAnneal") == 0 ) {
				success = 1;
				ExampleAnnealTSP(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"KnapsackAnneal") == 0 ) {
				success = 1;
				ExampleAnnealKnapsack(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"LinearRegressionExample") == 0 ) {
				success = 1;
				ExampleLinearRegression(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"GLMExample") == 0 ) {
				success = 1;
				ExampleGLM(i+1,argc,argv);
			} else if( strcasecmp(argv[i],"test") == 0 ) {
				success = 1;
				ExampleTest(i+1,argc,argv);
			}
		}
	}

	if( !success ) {
		printf("Usage:\n%s [-pause] ExampleName [arg1] [args2] ...\n",argv[0]);
		printf("\nWhere ExampleName is one of:\n");
		printf("** Chapter 1: Introduction to AI, Examples:\n");
		printf("CSVExample : Shows how to use CsvHelper to read a CSV\n");
		printf("\n");
		printf("** Chapter 2: Normalizing Data, Examples:\n");
		printf("NormalizeCSVExample : A simple normalization example for the Iris data set\n");
		printf("SimpleNormalize : Perform a few very basic normalizations\n");
		printf("Analyze : Analyze the Iris data set\n");
		printf("\n");
		printf("** Chapter 3: Distance Metrics, Examples:\n");
		printf("Distance : See various distance metrics\n");
		printf("\n");
		printf("** Chapter 4: Random Numbers, Examples:\n");
		printf("Random : Compare PRNG's and distributions\n");
		printf("EvaluatePI : Approximate PI by Monte Carlo\n");
		printf("\n");
		printf("** Chapter 5: K-Means, Examples:\n");
		printf("PerformCluster : Attempt to cluster the iris data set\n");
		printf("\n");
		printf("** Chapter 6: Error Calculation, Examples:\n");
		printf("EvaluateErrors : Evaluate several error calculation methods\n");
		printf("\n");
		printf("** Chapter 7: Towards Machine Learning, Examples:\n");
		printf("LearnIris : Learn Iris data using RBF network & Greedy Random algorithm\n");
		printf("LearnPolynomial : Learn a simple polynomial with the Greedy Random algorithm\n");
		printf("LearnXOR : Learn the XOR function with a RBF Network trained by Greedy Random\n");
		printf("\n");
		printf("** Chapter 8: Optimization Algorithms, Examples:\n");
		printf("LearnIrisAnneal : Learn the Iris data set w/RBF net & annealing\n");
		printf("LearnIrisClimb : Learn the Iris data set w/RBF net & hill climbing\n");
		printf("LearnIrisNelderMead : Learn the Iris data set w/RBF net & Nelder Mead\n");
		printf("\n");
		printf("** Chapter 9: Discrete Optimization, Examples:\n");
		printf("KnapsackAnneal : Knapsack optimization with Simulated Annealing\n");
		printf("TravelingSalesmanAnneal : Traveling Salesman with Simulated Annealing\n");
		printf("\n");
		printf("** Chapter 10: Linear Regression, Examples:\n");
		printf("GLMExample : Use a GLM to predict the probability of breast cancer\n");
		printf("LinearRegressionExample : Linear regression on the abalone data set\n");
	}

	if( pause ) {
		printf("[Press Any Key to Exit]");
		getchar();
	}

	return 0;
}

