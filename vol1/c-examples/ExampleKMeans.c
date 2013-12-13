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

/*
This example shows how to use KMeans clustering on iris data set.  There is not enough information in the
iris data set to perfectly cluster the  flowers.  However, you will notice that species will
mostly cluster together.

Iteration #1
Iteration #2
Iteration #3
Iteration #4
Iteration #5
Iteration #6
Iteration #7
Iteration #8
Iteration #9
Iteration #10
Cluster #1(6.853846,3.076923,5.715385,2.053846)
[6.200000,3.400000,5.400000,2.300000] -> Iris-virginica
[6.500000,3.000000,5.200000,2.000000] -> Iris-virginica
[6.700000,3.000000,5.200000,2.300000] -> Iris-virginica
[6.700000,3.300000,5.700000,2.500000] -> Iris-virginica
[6.800000,3.200000,5.900000,2.300000] -> Iris-virginica
[6.900000,3.100000,5.100000,2.300000] -> Iris-virginica
[6.700000,3.100000,5.600000,2.400000] -> Iris-virginica
[6.900000,3.100000,5.400000,2.100000] -> Iris-virginica
[6.400000,3.100000,5.500000,1.800000] -> Iris-virginica
[6.300000,3.400000,5.600000,2.400000] -> Iris-virginica
[7.700000,3.000000,6.100000,2.300000] -> Iris-virginica
[6.100000,2.600000,5.600000,1.400000] -> Iris-virginica
[6.400000,2.800000,5.600000,2.200000] -> Iris-virginica
[7.900000,3.800000,6.400000,2.000000] -> Iris-virginica
[7.400000,2.800000,6.100000,1.900000] -> Iris-virginica
[7.200000,3.000000,5.800000,1.600000] -> Iris-virginica
[6.400000,2.800000,5.600000,2.100000] -> Iris-virginica
[7.200000,3.200000,6.000000,1.800000] -> Iris-virginica
[6.700000,3.300000,5.700000,2.100000] -> Iris-virginica
[7.700000,2.800000,6.700000,2.000000] -> Iris-virginica
[6.900000,3.200000,5.700000,2.300000] -> Iris-virginica
[7.700000,2.600000,6.900000,2.300000] -> Iris-virginica
[7.700000,3.800000,6.700000,2.200000] -> Iris-virginica
[6.500000,3.000000,5.500000,1.800000] -> Iris-virginica
[6.400000,3.200000,5.300000,2.300000] -> Iris-virginica
[6.800000,3.000000,5.500000,2.100000] -> Iris-virginica
[6.400000,2.700000,5.300000,1.900000] -> Iris-virginica
[6.500000,3.200000,5.100000,2.000000] -> Iris-virginica
[7.200000,3.600000,6.100000,2.500000] -> Iris-virginica
[6.700000,2.500000,5.800000,1.800000] -> Iris-virginica
[7.300000,2.900000,6.300000,1.800000] -> Iris-virginica
[7.600000,3.000000,6.600000,2.100000] -> Iris-virginica
[6.500000,3.000000,5.800000,2.200000] -> Iris-virginica
[6.300000,2.900000,5.600000,1.800000] -> Iris-virginica
[7.100000,3.000000,5.900000,2.100000] -> Iris-virginica
[6.300000,3.300000,6.000000,2.500000] -> Iris-virginica
[6.700000,3.000000,5.000000,1.700000] -> Iris-versicolor
[6.900000,3.100000,4.900000,1.500000] -> Iris-versicolor
[7.000000,3.200000,4.700000,1.400000] -> Iris-versicolor
Cluster #2(5.883607,2.740984,4.388525,1.434426)
[5.800000,2.800000,5.100000,2.400000] -> Iris-virginica
[6.800000,2.800000,4.800000,1.400000] -> Iris-versicolor
[6.300000,2.800000,5.100000,1.500000] -> Iris-virginica
[6.300000,2.500000,5.000000,1.900000] -> Iris-virginica
[6.700000,3.100000,4.700000,1.500000] -> Iris-versicolor
[6.300000,2.700000,4.900000,1.800000] -> Iris-virginica
[5.900000,3.000000,5.100000,1.800000] -> Iris-virginica
[6.000000,2.700000,5.100000,1.600000] -> Iris-versicolor
[5.800000,2.700000,5.100000,1.900000] -> Iris-virginica
[6.100000,3.000000,4.900000,1.800000] -> Iris-virginica
[5.800000,2.700000,5.100000,1.900000] -> Iris-virginica
[6.300000,3.300000,4.700000,1.600000] -> Iris-versicolor
[6.300000,2.500000,4.900000,1.500000] -> Iris-versicolor
[6.200000,2.800000,4.800000,1.800000] -> Iris-virginica
[6.000000,3.000000,4.800000,1.800000] -> Iris-virginica
[6.500000,2.800000,4.600000,1.500000] -> Iris-versicolor
[6.600000,2.900000,4.600000,1.300000] -> Iris-versicolor
[6.700000,3.100000,4.400000,1.400000] -> Iris-versicolor
[5.900000,3.200000,4.800000,1.800000] -> Iris-versicolor
[5.700000,2.500000,5.000000,2.000000] -> Iris-virginica
[6.400000,3.200000,4.500000,1.500000] -> Iris-versicolor
[6.600000,3.000000,4.400000,1.400000] -> Iris-versicolor
[6.000000,2.200000,5.000000,1.500000] -> Iris-virginica
[5.600000,2.800000,4.900000,2.000000] -> Iris-virginica
[6.100000,2.900000,4.700000,1.400000] -> Iris-versicolor
[6.000000,3.400000,4.500000,1.600000] -> Iris-versicolor
[6.100000,3.000000,4.600000,1.400000] -> Iris-versicolor
[6.200000,2.200000,4.500000,1.500000] -> Iris-versicolor
[6.100000,2.800000,4.700000,1.200000] -> Iris-versicolor
[6.400000,2.900000,4.300000,1.300000] -> Iris-versicolor
[6.000000,2.900000,4.500000,1.500000] -> Iris-versicolor
[6.300000,2.300000,4.400000,1.300000] -> Iris-versicolor
[6.200000,2.900000,4.300000,1.300000] -> Iris-versicolor
[5.700000,2.800000,4.500000,1.300000] -> Iris-versicolor
[5.900000,3.000000,4.200000,1.500000] -> Iris-versicolor
[5.600000,3.000000,4.500000,1.500000] -> Iris-versicolor
[5.800000,2.700000,4.100000,1.000000] -> Iris-versicolor
[6.100000,2.800000,4.000000,1.300000] -> Iris-versicolor
[5.400000,3.000000,4.500000,1.500000] -> Iris-versicolor
[5.600000,3.000000,4.100000,1.300000] -> Iris-versicolor
[5.500000,2.600000,4.400000,1.200000] -> Iris-versicolor
[5.600000,2.700000,4.200000,1.300000] -> Iris-versicolor
[5.700000,3.000000,4.200000,1.200000] -> Iris-versicolor
[5.700000,2.900000,4.200000,1.300000] -> Iris-versicolor
[5.700000,2.800000,4.100000,1.300000] -> Iris-versicolor
[4.900000,2.500000,4.500000,1.700000] -> Iris-virginica
[5.100000,2.500000,3.000000,1.100000] -> Iris-versicolor
[5.800000,2.600000,4.000000,1.200000] -> Iris-versicolor
[5.500000,2.500000,4.000000,1.300000] -> Iris-versicolor
[5.800000,2.700000,3.900000,1.200000] -> Iris-versicolor
[5.500000,2.400000,3.700000,1.000000] -> Iris-versicolor
[5.500000,2.400000,3.800000,1.100000] -> Iris-versicolor
[5.700000,2.600000,3.500000,1.000000] -> Iris-versicolor
[5.600000,2.500000,3.900000,1.100000] -> Iris-versicolor
[5.600000,2.900000,3.600000,1.300000] -> Iris-versicolor
[6.000000,2.200000,4.000000,1.000000] -> Iris-versicolor
[5.000000,2.000000,3.500000,1.000000] -> Iris-versicolor
[5.200000,2.700000,3.900000,1.400000] -> Iris-versicolor
[4.900000,2.400000,3.300000,1.000000] -> Iris-versicolor
[5.500000,2.300000,4.000000,1.300000] -> Iris-versicolor
[5.000000,2.300000,3.300000,1.000000] -> Iris-versicolor
Cluster #3(5.006000,3.418000,1.464000,0.244000)
[5.000000,3.300000,1.400000,0.200000] -> Iris-setosa
[5.300000,3.700000,1.500000,0.200000] -> Iris-setosa
[4.600000,3.200000,1.400000,0.200000] -> Iris-setosa
[5.100000,3.800000,1.600000,0.200000] -> Iris-setosa
[4.800000,3.000000,1.400000,0.300000] -> Iris-setosa
[5.100000,3.800000,1.900000,0.400000] -> Iris-setosa
[5.000000,3.500000,1.600000,0.600000] -> Iris-setosa
[4.400000,3.200000,1.300000,0.200000] -> Iris-setosa
[4.500000,2.300000,1.300000,0.300000] -> Iris-setosa
[5.000000,3.500000,1.300000,0.300000] -> Iris-setosa
[5.100000,3.400000,1.500000,0.200000] -> Iris-setosa
[4.400000,3.000000,1.300000,0.200000] -> Iris-setosa
[4.900000,3.100000,1.500000,0.100000] -> Iris-setosa
[5.500000,3.500000,1.300000,0.200000] -> Iris-setosa
[5.000000,3.200000,1.200000,0.200000] -> Iris-setosa
[4.900000,3.100000,1.500000,0.100000] -> Iris-setosa
[5.200000,4.100000,1.500000,0.100000] -> Iris-setosa
[5.400000,3.400000,1.500000,0.400000] -> Iris-setosa
[4.800000,3.100000,1.600000,0.200000] -> Iris-setosa
[4.700000,3.200000,1.600000,0.200000] -> Iris-setosa
[5.200000,3.400000,1.400000,0.200000] -> Iris-setosa
[5.200000,3.500000,1.500000,0.200000] -> Iris-setosa
[5.000000,3.400000,1.600000,0.400000] -> Iris-setosa
[5.000000,3.000000,1.600000,0.200000] -> Iris-setosa
[4.800000,3.400000,1.900000,0.200000] -> Iris-setosa
[5.100000,3.300000,1.700000,0.500000] -> Iris-setosa
[4.600000,3.600000,1.000000,0.200000] -> Iris-setosa
[5.100000,3.700000,1.500000,0.400000] -> Iris-setosa
[5.400000,3.400000,1.700000,0.200000] -> Iris-setosa
[5.100000,3.800000,1.500000,0.300000] -> Iris-setosa
[5.700000,3.800000,1.700000,0.300000] -> Iris-setosa
[5.100000,3.500000,1.400000,0.300000] -> Iris-setosa
[5.400000,3.900000,1.300000,0.400000] -> Iris-setosa
[5.700000,4.400000,1.500000,0.400000] -> Iris-setosa
[5.800000,4.000000,1.200000,0.200000] -> Iris-setosa
[4.300000,3.000000,1.100000,0.100000] -> Iris-setosa
[4.800000,3.000000,1.400000,0.100000] -> Iris-setosa
[4.800000,3.400000,1.600000,0.200000] -> Iris-setosa
[5.400000,3.700000,1.500000,0.200000] -> Iris-setosa
[4.900000,3.100000,1.500000,0.100000] -> Iris-setosa
[4.400000,2.900000,1.400000,0.200000] -> Iris-setosa
[5.000000,3.400000,1.500000,0.200000] -> Iris-setosa
[4.600000,3.400000,1.400000,0.300000] -> Iris-setosa
[5.400000,3.900000,1.700000,0.400000] -> Iris-setosa
[5.000000,3.600000,1.400000,0.200000] -> Iris-setosa
[4.600000,3.100000,1.500000,0.200000] -> Iris-setosa
[4.700000,3.200000,1.300000,0.200000] -> Iris-setosa
[4.900000,3.000000,1.400000,0.200000] -> Iris-setosa
[5.100000,3.500000,1.400000,0.200000] -> Iris-setosa
[5.500000,4.200000,1.400000,0.200000] -> Iris-setosa

*/
void ExampleKMeans(int argIndex, int argc, char **argv) {	
	char filename[FILENAME_MAX];
	CLUSTER_ALOG *kmeans;
	CLUSTER_ITEM *dataset;
	int iteration;

	LocateFile("iris.csv",filename,FILENAME_MAX);
	kmeans = CreateKMeans(3,4);
	dataset = KMeansLoadCSV(filename,4,0,4);

	/* Note, it is not necessary to delete the dataset list, it will be deleted
	   when the clusters are deleted.  However, if you did need to delete it, pre-clustering,
	   the following command would do that.

		KMeansDeleteList(dataset); */

	/* To display the dataset, pre-clustering, use: 
	
	   KMeansDumpList(stdout,dataset,4); */

	
	KMeansInitForgy(kmeans,dataset);
	
	iteration = 1;
	while(!KMeansIteration(kmeans)) {
		printf("Iteration #%i\n",iteration);
		iteration++;
	}
	KMeansDump(stdout,kmeans);

	DeleteKMeans(kmeans);
}