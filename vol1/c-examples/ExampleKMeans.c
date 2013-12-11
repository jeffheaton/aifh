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