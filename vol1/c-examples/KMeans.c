#include "aifh-vol1.h"

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
	strncpy(result->label,label,sizeof(label)+1);
	result->next = NULL;
	return result;
}

void DeleteClusterItem(CLUSTER_ITEM *item) {
	free(item);
}

CLUSTER_ALOG *CreateKMeans(int k,int featureCount) {
	CLUSTER_ALOG *result;
	
	result = (CLUSTER_ALOG *)calloc(1,sizeof(CLUSTER_ALOG));
	result->k = k;
	result->featureCount = featureCount;
	result->clusters = (CLUSTER*)calloc(k,sizeof(CLUSTER));
	result->rnd = RandCreate(TYPE_RANDOM_MT,(long)time(NULL));
	result->dist = &DistanceEuclidean;
	return result;
}

int KMeansCountItems(CLUSTER_ITEM *first) {
	CLUSTER_ITEM *currentItem;
	int result;

	currentItem = first;
	result = 0;

	while(first!=NULL) {
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
	}
}

void KMeansInitRandom(CLUSTER_ALOG *kmeans, CLUSTER_ITEM *items) {
	CLUSTER *currentCluster;
	CLUSTER *sourceCluster;
	int currentClusterIndex;
	CLUSTER_ITEM *currentItem;
	CLUSTER_ITEM *nextItem;
	CLUSTER_ITEM *sourceItem;
	int sourceClusterIndex;
	int sourceClusterCount;
	int sourceItemIndex;

	/* First, assign all items to a random cluster */
	currentItem = items;
	while(currentItem!=NULL) {
		nextItem = currentItem->next;
		currentClusterIndex = RandNextIntRange(kmeans->rnd, 0,kmeans->k);
		currentItem->next = kmeans->clusters[currentClusterIndex].firstItem;
		kmeans->clusters[currentClusterIndex].firstItem = nextItem;
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
}	

CLUSTER *KMeansFindNearestCluster(CLUSTER_ALOG *kmeans, CLUSTER_ITEM *item) {
	CLUSTER *currentCluster;
	CLUSTER *result;
	int currentClusterIndex;
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
	int currentClusterIndex;
	CLUSTER *currentCluster;
	int itemCount;
	int selectedItemIndex;
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
}

void KMeansUpdateStep(CLUSTER_ALOG *kmeans) {
	int currentClusterIndex;
	CLUSTER *currentCluster;
	CLUSTER_ITEM *currentItem;
	int i;
	int itemCount;

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
	int currentClusterIndex;
	CLUSTER *currentCluster;
	CLUSTER *nearestCluster;
	CLUSTER_ITEM *currentItem,*nextItem;
	int done;

	/* Loop over every cluster */
	done = 1;
	unassignedItems = NULL;
	for(currentClusterIndex=0;currentClusterIndex<kmeans->k;currentClusterIndex++) {
		currentCluster = &kmeans->clusters[currentClusterIndex];
		
		/* loop over all items in this cluster */
		currentItem = currentCluster->firstItem;
		while(currentItem!=NULL) {
			nextItem = currentItem->next;
			nearestCluster = KMeansFindNearestCluster(kmeans,nextItem);
			/* did we move */
			if( nearestCluster!=currentCluster ) {
				done = 0;
				/* Perform the move */
				KMeansRemoveItem(&currentCluster->firstItem,currentItem);
				currentItem->next = nearestCluster->firstItem;
				nearestCluster->firstItem = currentItem;
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

CLUSTER_ITEM* KMeansLoadCSV(char *filename, int labelColumn, int startColumn, int endColumn) {
	return NULL;
}

void KMeansDumpList(FILE *out, CLUSTER_ITEM *first) {
}

void KMeansD 