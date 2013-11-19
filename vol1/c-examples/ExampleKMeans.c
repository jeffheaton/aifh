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