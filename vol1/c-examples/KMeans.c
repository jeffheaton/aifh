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
#include "aifh-vol1.h"

struct _KMeansStructCSV {
	long unsigned col;
	long unsigned row;
	unsigned int labelCol;
	unsigned int featureCount;
	unsigned int startCol;
	double *features;
	CLUSTER_ITEM *item;
	CLUSTER_ITEM *prevItem;
	CLUSTER_ITEM *firstItem;
} _KMeansStructCSV;

static void _KMeansCallbackColumn (void *s, size_t len, void *data) 
{
	struct _KMeansStructCSV *csv;

	csv = (struct _KMeansStructCSV*)data;

	if( csv->row>0 ) {
		/* is this the label column */
		if( csv->col==csv->labelCol ) {			
			/* attach the item to the list */
			csv->prevItem = csv->item;
			csv->item = CreateClusterItem(csv->featureCount,(char*)s);
			csv->item->next = NULL;
			/* do we need to link to a previous item (usually the case) */
			if( csv->prevItem!=NULL ) {
				csv->prevItem->next = csv->item;
			}
			/* is this the first item */
			if( csv->firstItem==NULL ) {
				csv->firstItem = csv->item;
			}
		}
		/* is this a feature column */
		else if( csv->col>=csv->startCol && csv->col<(csv->startCol+csv->featureCount) ) {
			csv->features[csv->col-csv->startCol] = atof((char*)s);
		}
	}
	
	csv->col++;
}
static void _KMeansCallbackRow (int c, void *data) 
{
	struct _KMeansStructCSV *csv;
	unsigned int i;

	csv = (struct _KMeansStructCSV*)data;	
	csv->col= 0;
	if( csv->item != NULL ) {
		for(i=0;i<csv->featureCount;i++) {
			csv->item->features[i] = csv->features[i];
		}
	}

	csv->row++;
}

CLUSTER_ITEM *CreateClusterItem(int featureCount, char *label) {
	int size;
	CLUSTER_ITEM *result;
	unsigned char *bytes;

	/* Calculate the size of the object.  This is the size of the struct header, plus 
	   enough space for all of the features and the label. */
	size = sizeof(CLUSTER_ITEM) + (featureCount*sizeof(double)) + strlen(label) + 1;
	result = (CLUSTER_ITEM *)calloc(1,size);
	bytes = (unsigned char*)&result->data;
	
	result->features = (double*)bytes;
	result->label = (char*)(bytes+(sizeof(double)*featureCount));
	strncpy(result->label,label,strlen(label)+1);
	result->next = NULL;
	return result;
}

void DeleteClusterItem(CLUSTER_ITEM *item) {
	free(item);
}

CLUSTER_ALOG *CreateKMeans(int k,int featureCount) {
	CLUSTER_ALOG *result;
	int i;
	
	result = (CLUSTER_ALOG *)calloc(1,sizeof(CLUSTER_ALOG));
	result->k = k;
	result->featureCount = featureCount;
	result->clusters = (CLUSTER*)calloc(k,sizeof(CLUSTER));
	result->rnd = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
	result->dist = &DistanceEuclidean;

	/* allocate centroids */
	for(i=0;i<k;i++) {
		result->clusters[i].centroid = (double*)calloc(featureCount,sizeof(double));
	}
	return result;
}

int KMeansCountItems(CLUSTER_ITEM *first) {
	CLUSTER_ITEM *currentItem;
	int result;

	currentItem = first;
	result = 0;

	while(currentItem!=NULL) {
		result++;
		currentItem = currentItem->next;
	}
	return result;
}

CLUSTER_ITEM *KMeansFindItem(CLUSTER_ITEM *first, int index) {
	CLUSTER_ITEM *result;
	CLUSTER_ITEM *currentItem;
	int i=0;

	result = NULL;
	currentItem = first;

	while(currentItem!=NULL) {
		if( i==index ) {
			return currentItem;
		}
		i++;
		currentItem = currentItem->next;
	}

	return NULL;
}

void KMeansRemoveItem(CLUSTER_ITEM **first, CLUSTER_ITEM *targetItem) {
	CLUSTER_ITEM *currentItem;

	/* Are we removing the first item?  Special case */
	if( *first==targetItem ) {
		*first = (*first)->next;
		return;
	}

	/* Walk the list and find the item. */
	currentItem = *first;
	while(currentItem!=NULL) {
		if( currentItem->next == targetItem ) {
			currentItem->next = currentItem->next->next;
		}
		currentItem=currentItem->next;
	}
}

void KMeansInitRandom(CLUSTER_ALOG *kmeans, CLUSTER_ITEM *items) {
	CLUSTER *currentCluster;
	CLUSTER *sourceCluster;
	unsigned int currentClusterIndex;
	CLUSTER_ITEM *currentItem;
	CLUSTER_ITEM *nextItem;
	CLUSTER_ITEM *sourceItem;
	unsigned int sourceClusterIndex;
	unsigned int sourceClusterCount;
	unsigned int sourceItemIndex;

	/* First, assign all items to a random cluster */
	currentItem = items;
	while(currentItem!=NULL) {
		nextItem = currentItem->next;
		currentClusterIndex = RandNextIntRange(kmeans->rnd, 0,kmeans->k);
		currentItem->next = kmeans->clusters[currentClusterIndex].firstItem;
		kmeans->clusters[currentClusterIndex].firstItem = currentItem;
		currentItem = nextItem;
	}

	/* Finally, make sure that all clusters have at least one item */
	for(currentClusterIndex=0;currentClusterIndex<kmeans->k;currentClusterIndex++) {
		currentCluster = &kmeans->clusters[currentClusterIndex];
		while(currentCluster->firstItem==NULL) {
			/* Choose a random cluster, might be the same cluster we are trying to fill.  */
			sourceClusterIndex = RandNextIntRange(kmeans->rnd, 0,kmeans->k);
			sourceCluster = &kmeans->clusters[sourceClusterIndex];
			sourceClusterCount = KMeansCountItems(sourceCluster->firstItem);
			/* Does J have at least two items? (this also means we did not pick the cluster we are filling)*/
			if( sourceClusterCount>=2 ) {
				sourceItemIndex = RandNextIntRange(kmeans->rnd, 0, sourceClusterCount);
				sourceItem = KMeansFindItem(sourceCluster->firstItem,sourceClusterIndex);
				KMeansRemoveItem(&sourceCluster->firstItem,sourceItem);
				sourceItem->next = NULL;
				currentCluster->firstItem = sourceItem;
			}
		}
	}

	/* Calculate the centroids */
	KMeansUpdateStep(kmeans);
}	

CLUSTER *KMeansFindNearestCluster(CLUSTER_ALOG *kmeans, CLUSTER_ITEM *item) {
	CLUSTER *currentCluster;
	CLUSTER *result;
	unsigned int currentClusterIndex;
	double minDist, dist;

	/* Loop over all clusters and check the centroid of each to the passed in item */
	result = NULL;
	for(currentClusterIndex=0;currentClusterIndex<kmeans->k;currentClusterIndex++) {
		currentCluster = &kmeans->clusters[currentClusterIndex];
		/* Calculate the distance between the item and this cluster's centroid */
		dist = kmeans->dist(currentCluster->centroid,0,item->features,0,kmeans->featureCount);
		/* Do we have a new best? */
		if( result==NULL || dist<minDist ) {
			minDist = dist;
			result = currentCluster;
		}
	}

	return result;
}

void KMeansInitForgy(CLUSTER_ALOG *kmeans, CLUSTER_ITEM *items) {
	unsigned int currentClusterIndex;
	CLUSTER *currentCluster;
	unsigned int itemCount;
	unsigned int selectedItemIndex;
	CLUSTER_ITEM *selectedItem;
	CLUSTER_ITEM *currentItem;
	CLUSTER_ITEM *nextItem;
	CLUSTER *nearestCluster;

	/* Assign a random item to each cluster */
	itemCount = KMeansCountItems(items);
	for(currentClusterIndex=0;currentClusterIndex<kmeans->k;currentClusterIndex++) {
		currentCluster = &kmeans->clusters[currentClusterIndex];
		selectedItemIndex = RandNextIntRange(kmeans->rnd, 0, itemCount);
		selectedItem = KMeansFindItem(items,selectedItemIndex);
		KMeansRemoveItem(&items,selectedItem);
		selectedItem->next = NULL;
		itemCount--;
		currentCluster->firstItem=selectedItem;
		memcpy(currentCluster->centroid,selectedItem->features,sizeof(double)*kmeans->featureCount);
	}

	/* Assign remaining items to values */
	currentItem = items;
	while(currentItem!=NULL) {
		nextItem = currentItem->next;

		/* Find nearest cluster */
		nearestCluster = KMeansFindNearestCluster(kmeans,currentItem);

		/* Add item to nearest cluster */
		currentItem->next = nearestCluster->firstItem;
		nearestCluster->firstItem = currentItem;

		/* Move to next item */
		currentItem = nextItem;
	}

	/* Calculate the centroids */
	KMeansUpdateStep(kmeans);
}

void KMeansUpdateStep(CLUSTER_ALOG *kmeans) {
	unsigned int currentClusterIndex;
	CLUSTER *currentCluster;
	CLUSTER_ITEM *currentItem;
	unsigned int i;
	unsigned int itemCount;

	/* Loop over every cluster */
	for(currentClusterIndex=0;currentClusterIndex<kmeans->k;currentClusterIndex++) {
		currentCluster = &kmeans->clusters[currentClusterIndex];

		/* Clear the centroid */
		for(i=0;i<kmeans->featureCount;i++) {
			currentCluster->centroid[i] = 0;
		}

		/* Loop over items and sum, so that we can take the mean */
		currentItem = currentCluster->firstItem;
		itemCount=0;
		while(currentItem!=NULL) {
			/* Sum into the centroid */
			for(i=0;i<kmeans->featureCount;i++) {
				currentCluster->centroid[i] += currentItem->features[i];
			}
			itemCount++;
			currentItem = currentItem->next;
		}

		/* Mean of the centroid */
		for(i=0;i<kmeans->featureCount;i++) {
			currentCluster->centroid[i] /= (double)itemCount;
		}
	}
}

int KMeansAssignStep(CLUSTER_ALOG *kmeans) {
	CLUSTER_ITEM *unassignedItems;
	unsigned int currentClusterIndex;
	CLUSTER *currentCluster;
	CLUSTER *nearestCluster;
	CLUSTER_ITEM *currentItem,*nextItem;
	unsigned int done;
	unsigned int clusterCount;

	/* Loop over every cluster */
	done = 1;
	unassignedItems = NULL;
	for(currentClusterIndex=0;currentClusterIndex<kmeans->k;currentClusterIndex++) {
		currentCluster = &kmeans->clusters[currentClusterIndex];
		
		clusterCount = KMeansCountItems(currentCluster->firstItem);
		/* loop over all items in this cluster, make sure to leave at least one */
		currentItem = currentCluster->firstItem;
		while(currentItem!=NULL && clusterCount>1) {
			nextItem = currentItem->next;
			nearestCluster = KMeansFindNearestCluster(kmeans,currentItem);
			/* did we move */
			if( nearestCluster!=currentCluster ) {
				done = 0;
				/* Perform the move */
				KMeansRemoveItem(&currentCluster->firstItem,currentItem);
				currentItem->next = nearestCluster->firstItem;
				nearestCluster->firstItem = currentItem;
				clusterCount--;
			}
			currentItem = nextItem;
		}
	}

	return done;
}

int KMeansIteration(CLUSTER_ALOG *kmeans) {
	int done;

	done = KMeansAssignStep(kmeans);

	if (!done) {
		KMeansUpdateStep(kmeans);
	}

	return done;
}

CLUSTER_ITEM* KMeansLoadCSV(char *filename, int labelColumn, int startColumn, int featureCount) {
	FILE *fp;
	char buf[1024];
	size_t bytes_read;
	struct _KMeansStructCSV c;
	struct csv_parser p;
	CLUSTER_ITEM *result;

	/* Setup csvlib to read the CSV file */
	if (csv_init(&p, CSV_APPEND_NULL) != 0) exit(EXIT_FAILURE);
	fp = fopen(filename, "rb");
	if (!fp)
	{ 
		printf("Could not open: %s\n", filename);
		exit(EXIT_FAILURE); 
	}

	c.row = 0;
	c.col = 0;
	c.startCol = startColumn;
	c.featureCount = featureCount;
	c.labelCol = labelColumn;
	c.item = c.prevItem = c.firstItem = NULL;
	c.features = (double*)calloc(featureCount,sizeof(double));

	/* Loop over the contents.  It is important to note that we are not reading line by
	   line, at this level.  Rather, we are passing blocks off to csvlib.  Then csvlib
	   calls our two callbacks as needed. */

	while ((bytes_read=fread(buf, 1, 1024, fp)) > 0) {
		if (csv_parse(&p, buf, bytes_read, _KMeansCallbackColumn, _KMeansCallbackRow, &c) != bytes_read) {
			fprintf(stderr, "Error while parsing file: %s\n",
			csv_strerror(csv_error(&p)) );
			exit(EXIT_FAILURE);
		}
	}

	result = c.firstItem;

	/* Handle any final data.  May call the callbacks once more */
	csv_fini(&p, _KMeansCallbackColumn, _KMeansCallbackRow, &c);


	/* Cleanup */
	free(c.features);
	fclose(fp);
	csv_free(&p);

	return result;
}

void KMeansDumpList(FILE *out, CLUSTER_ITEM *first, int featureCount) {
	CLUSTER_ITEM *currentItem;
	int i;

	currentItem = first;
	while(currentItem!=NULL) {
		fprintf(out,"[");
		for(i=0;i<featureCount;i++) {
			if( i!=0 ) {
				fprintf(out,",");
			}
			fprintf(out,"%f",currentItem->features[i]);
		}
		fprintf(out,"] -> %s\n",currentItem->label);
		currentItem = currentItem->next;
	}
}

void KMeansDump(FILE *out, CLUSTER_ALOG *alog) {
	CLUSTER *cluster;
	unsigned int i,j;

	for(i=0;i<alog->k;i++) {
		cluster = &alog->clusters[i];
		fprintf(out,"Cluster #%i(",i+1);
		for(j=0;j<alog->featureCount;j++) {
			if( j>0 ) {
				fprintf(out,",");
			}
			fprintf(out,"%f",cluster->centroid[j]);
		}
		fprintf(out,")\n");
		KMeansDumpList(out,cluster->firstItem,alog->featureCount);
	}
}

void DeleteKMeansList(CLUSTER_ITEM *first) {
	CLUSTER_ITEM *currentItem;
	CLUSTER_ITEM *nextItem;

	currentItem = first;

	while(currentItem!=NULL) {
		nextItem = currentItem->next;
		DeleteKMeansItem(currentItem);
		currentItem = nextItem;
	}

}

void DeleteKMeansItem(CLUSTER_ITEM *item) {
	free(item);
}

void DeleteKMeans(CLUSTER_ALOG *alog) {
	CLUSTER *cluster;
	unsigned int i;

	for(i=0;i<alog->k;i++) {
		cluster = &alog->clusters[i];
		DeleteKMeansList(cluster->firstItem);
		free(cluster->centroid);
	}
}
