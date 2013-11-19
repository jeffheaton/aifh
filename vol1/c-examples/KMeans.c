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
	CLUSTER *sourceCluster;
	CLUSTER_ITEM *currentItem;
	CLUSTER_ITEM *moveItem;
	CLUSTER_ITEM *nextItem;
	int i,j;

	/* First, assign all items to a random cluster */
	currentItem = items;
	while(currentItem!=NULL) {
		nextItem = currentItem->next;
		i = RandNextIntRange(kmeans->rnd, 0,kmeans->k);
		currentItem->next = kmeans->clusters[i].firstItem;
		kmeans->clusters[i].firstItem = nextItem;
		currentItem = nextItem;
	}

	/* Finally, make sure that all clusters have at least one item */
	for(i=0;i<kmeans->k;i++) {
		sourceCluster = &kmeans->clusters[i];
		while(sourceCluster->firstItem==NULL) {
			/* Choose a random cluster, might be the same cluster we are trying to fill.  */
			j = RandNextIntRange(kmeans->rnd, 0,kmeans->k);
			/* Does J have at least two items? (this also means we did not pick the cluster we are filling)*/
			if( kmeans->clusters[j].firstItem!=NULL && kmeans->clusters[j].firstItem->next!=NULL ) {
				/* Move an item from J to the empty cluster */
				moveItem = kmeans->clusters[j].firstItem;
				kmeans->clusters[j].firstItem = moveItem->next;
				moveItem->next = NULL;
				sourceCluster->firstItem = moveItem;
			}
		}
	}
}	

void KMeansInitForgy(CLUSTER_ALOG *kmeans, CLUSTER_ITEM *items) {

}