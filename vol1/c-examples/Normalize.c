#include "aifh-vol1.h"

typedef struct _PROCESS_PAIR {
	NORM_DATA *norm;
	DATA_SET *data;
	int rowCount;
} _PROCESS_PAIR;

static void _AppendUniqueClass(NORM_DATA_ITEM *item, char *className) {
	NORM_DATA_CLASS *lastClass;
	NORM_DATA_CLASS *newClass;
	int done;

	/* First scan the list and perform two functions */
	/* Function 1: Does this class already exist? */
	/* Function 2: Find the last item so that we can append later if the item does not exist */
	lastClass = item->firstClass;
	done=0;
	while(!done && lastClass!=NULL) {
		/* exit if we already have one */
		if(!strcmp(lastClass->name,className)) {
			return;
		}
		if( lastClass->next == NULL ) {
			done = 1;
		} else {
			lastClass=lastClass->next;
		}
	}

	/* Create a new class item */
	newClass = (NORM_DATA_CLASS*)calloc(1,sizeof(NORM_DATA_CLASS));
	newClass->name = strdup(className);

	/* Now add it */
	if( lastClass==NULL ) {
		/* New first item */
		item->firstClass = newClass;
	} else {
		lastClass->next = newClass;
	}

	/* Increase the class count */
	item->classCount++;
}

static void _AnalyzeCallbackColumn (void *s, size_t len, void *data) 
{
	NORM_DATA *norm;
	NORM_DATA_ITEM *col;
	int colNum;

	norm = (NORM_DATA *)data;

	/* Find the column definition */
	col = norm->firstItem;
	colNum = norm->_currentColumn;
	norm->_currentColumn++;

	while(colNum>0 && col!=NULL) {
		
		col = col->next;
		colNum--;
	}

	if( col==NULL ) {
		printf("The file had too many columns\n");
		exit(1);
	}

	/* Read the row */
	if(norm->rowCount==0 ) {
		/* Read header */
		col->name = strdup((char*)s);
		
	
	} else {
		/* Read regular data */
		double d = atof((char*)s);
		if( norm->rowCount==1 ) {
			col->actualHigh=d;
			col->actualLow=d;
		} else {
			switch(col->type) {
			case NORM_TYPE_PASS:
			case NORM_TYPE_RANGE:
				col->actualHigh=MAX(col->actualHigh,d);
				col->actualLow=MIN(col->actualLow,d);
				break;
			case NORM_CLASS_EQUILATERAL:
			case NORM_CLASS_ONEOFN:
				_AppendUniqueClass(col,(char*)s);
				break;
			}
		}
	}
}

static void _AnalyzeCallbackRow (int c, void *data) 
{
	NORM_DATA *norm;
	norm = (NORM_DATA *)data;
	norm->_currentColumn = 0;
	norm->rowCount++;
}

static void _ProcessCallbackColumn (void *s, size_t len, void *data) 
{
	_PROCESS_PAIR *pair;
	NORM_DATA *norm;
	NORM_DATA_ITEM *col;
	unsigned int colNum,i;
	double x;

	pair = (_PROCESS_PAIR*)data;
	norm = pair->norm;

	if( pair->rowCount==0) {
		return;
	}

	/* Find the column definition */
	col = norm->firstItem;
	colNum = norm->_currentColumn;
	norm->_currentColumn++;

	while(colNum>0 && col!=NULL) {
		
		col = col->next;
		colNum--;
	}

	if( col==NULL ) {
		printf("The file had too many columns\n");
		exit(1);
	}

	/* normalize the column */
	switch(col->type) {
		case NORM_TYPE_RANGE:
			x = atof((char*)s);
			*(pair->data->cursor++) = NormRange(col->actualLow, col->actualHigh, col->targetLow, col->targetHigh, x);
			break;
		case NORM_TYPE_REPLACE:
			i = (int)atof((char*)s);
			if( i==col->repSearchFor ) {
				*(pair->data->cursor++) = col->repReplaceWith;
			} else {
				*(pair->data->cursor++) = col->repOthers;
			}
			break;
		case NORM_TYPE_PASS:
			x = atof((char*)s);
			*(pair->data->cursor++) = x;
			break;
		case NORM_TYPE_IGNORE:
			break;
		case NORM_TYPE_RECIPROCAL:
			x = atof((char*)s);
			*(pair->data->cursor++) = NormReciprocal(x);
			break;
		case NORM_CLASS_ONEOFN:
			NormOneOfN(col->firstClass,col->targetLow,col->targetHigh,(char*)s,pair->data->cursor);
			pair->data->cursor+=col->classCount;
			break;
		case NORM_CLASS_EQUILATERAL:
			NormEquilateral(col->firstClass,col->equilateral,col->targetLow,col->targetHigh,(char*)s,pair->data->cursor);
			pair->data->cursor+=col->classCount-1;
			break;
	}
}

static void _ProcessCallbackRow (int c, void *data) 
{
	_PROCESS_PAIR *pair;
	int sz;

	pair = (_PROCESS_PAIR*)data;
	pair->rowCount++;
	pair->norm->_currentColumn = 0;
	sz = pair->data->cursor - pair->data->data;
}

static void _InitEquilateral(NORM_DATA *norm) {
	NORM_DATA_ITEM *current = norm->firstItem;

	while(current!=NULL) {
		if( current->type==NORM_CLASS_EQUILATERAL ) {
			if( current->equilateral !=NULL ) {
				free(current->equilateral);
			}
			current->equilateral = (double*)calloc(current->classCount * (current->classCount-1),sizeof(double));
			Equilat(current->classCount,current->targetLow,current->targetHigh,current->equilateral);
		}
		current = current->next;
	}
}

NORM_DATA *NormCreate() {
	NORM_DATA *result = (NORM_DATA *)calloc(1,sizeof(NORM_DATA));
	return result;
}

double NormRange(double dataLow, double dataHigh, double normalizedLow, double normalizedHigh, double x) {
	return ((x - dataLow) 
				/ (dataHigh - dataLow))
				* (normalizedHigh - normalizedLow) + normalizedLow;
}

double DeNormRange(double dataLow, double dataHigh, double normalizedLow, double normalizedHigh, double x) {
	return ((dataLow - dataHigh) * x - normalizedHigh
				* dataLow + dataHigh * normalizedLow)
				/ (normalizedLow - normalizedHigh);
}

double NormReciprocal(double x) {
	return 1/x;
}

double DeNormReciprocal(double x) {
	return 1/x;
}

void NormOneOfN(NORM_DATA_CLASS *first, double normalizedLow, double normalizedHigh, char *classX, double *dataOut) {
	NORM_DATA_CLASS *current;
	double *currentOut;

	current = first;
	currentOut = dataOut;
	while(current!=NULL) {
		if(!strcmp(current->name,classX) ) {
			*(currentOut++) = normalizedHigh;
		} else {
			*(currentOut++) = normalizedLow;
		}
		current = current->next;
	}
}

void NormEquilateral(NORM_DATA_CLASS *first, double *equilat, double normalizedLow, double normalizedHigh, char *classX, double *dataOut) {
	NORM_DATA_CLASS *current;
	int itemIndex = -1;
	int classCount = 0;
	int rowSize;

	/* find the item index */
	current = first;
	while(current!=NULL) {
		if(!strcmp(current->name,classX) ) {
			itemIndex = classCount;
		}
		classCount++;
		current = current->next;
	}

	/* did we find an index */
	if( itemIndex==-1 ) {
		printf("Invalid column label: %s\n", classX );
	}

	/* copy the correct equilat values */
	rowSize = classCount-1;
	memcpy(dataOut,equilat+(rowSize*itemIndex),rowSize*sizeof(double));
}

char* DeNormEquilateral(NORM_DATA_CLASS *first, double *equilat, int classCount, double normalizedLow, double normalizedHigh, double *dataOut) {
	NORM_DATA_CLASS *current;
	char * result;
	double minResult;
	int dataIndex;
	double dist;
	double *currentRow;

	current = first;
	result = NULL;
	dataIndex = 0;
	currentRow = equilat;

	while(current!=NULL) {
		dist = DistanceEuclidean(currentRow,0,dataOut,0,classCount);
		if( result==NULL || dist<minResult ) {
			minResult = dist;
			result = current->name;
		}

		current = current->next;
		currentRow+=(classCount-1);
		dataIndex++;
	}

	return result;
}

char* DeNormOneOfN(NORM_DATA_CLASS *first, double normalizedLow, double normalizedHigh, double *dataOut) {
	NORM_DATA_CLASS *current;
	char * result;
	double maxResult;
	int dataIndex;

	current = first;
	result = NULL;
	dataIndex = 0;

	while(current!=NULL) {
		if( result==NULL || dataOut[dataIndex]>maxResult ) {
			maxResult = dataOut[dataIndex];
			result = current->name;
		}

		current = current->next;
		dataIndex++;
	}

	return result;
}

void NormDelete(NORM_DATA *norm) {
	NORM_DATA_ITEM *item, *t;
	NORM_DATA_CLASS *cl,*tcl;

	item = norm->firstItem;
	while(item!=NULL) {
		t = (NORM_DATA_ITEM *)item->next;

		/* free any classes that this item might have */
		cl = item->firstClass;
		while(cl!=NULL) {
			tcl = (NORM_DATA_CLASS*)cl->next;
			free(cl);
			cl = tcl;
		}

		/* free the equilateral structure, if it exists */
		if( item->equilateral ) {
			free(item->equilateral);
		}

		/* free the actual item and its name */
		free(item->name);
		free(item);
		item = t;
	}

	free(norm);
}

void NormDefPass(NORM_DATA *norm) {
	NORM_DATA_ITEM *last = norm->firstItem;
	NORM_DATA_ITEM *newItem;

	/* Find the last item */ 

	while( last!=NULL && last->next != NULL ) {
		last = (NORM_DATA_ITEM *)last->next;
	}

	/* Create the new item */
	newItem = (NORM_DATA_ITEM*)calloc(1,sizeof(NORM_DATA_ITEM));
	newItem->type = NORM_TYPE_PASS;
	newItem->targetHigh = 0;
	newItem->targetLow = 0;
	newItem->next = NULL;

	/* Link the new item */
	if( last==NULL ) {
		/* Are we adding the first item */
		norm->firstItem = newItem;
	} else {
		/* Link to the end of the chain */
		last->next = (NORM_DATA_ITEM *)newItem;
	}
}

void NormDefIgnore(NORM_DATA *norm) {
	NORM_DATA_ITEM *last = norm->firstItem;
	NORM_DATA_ITEM *newItem;

	/* Find the last item */ 

	while( last!=NULL && last->next != NULL ) {
		last = (NORM_DATA_ITEM *)last->next;
	}

	/* Create the new item */
	newItem = (NORM_DATA_ITEM*)calloc(1,sizeof(NORM_DATA_ITEM));
	newItem->type = NORM_TYPE_IGNORE;
	newItem->targetHigh = 0;
	newItem->targetLow = 0;
	newItem->next = NULL;

	/* Link the new item */
	if( last==NULL ) {
		/* Are we adding the first item */
		norm->firstItem = newItem;
	} else {
		/* Link to the end of the chain */
		last->next = (NORM_DATA_ITEM *)newItem;
	}
}

void NormDefReplace(NORM_DATA *norm, int searchFor, int replaceWith, int others) {
	NORM_DATA_ITEM *last = norm->firstItem;
	NORM_DATA_ITEM *newItem;

	/* Find the last item */ 

	while( last!=NULL && last->next != NULL ) {
		last = (NORM_DATA_ITEM *)last->next;
	}

	/* Create the new item */
	newItem = (NORM_DATA_ITEM*)calloc(1,sizeof(NORM_DATA_ITEM));
	newItem->type = NORM_TYPE_REPLACE;
	newItem->targetHigh = 0;
	newItem->targetLow = 0;
	newItem->repOthers = others;
	newItem->repReplaceWith = replaceWith;
	newItem->repSearchFor = searchFor;
	newItem->next = NULL;

	/* Link the new item */
	if( last==NULL ) {
		/* Are we adding the first item */
		norm->firstItem = newItem;
	} else {
		/* Link to the end of the chain */
		last->next = (NORM_DATA_ITEM *)newItem;
	}
}

void NormDefRange(NORM_DATA *norm, double low, double high) {
	NORM_DATA_ITEM *last = norm->firstItem;
	NORM_DATA_ITEM *newItem;

	/* Find the last item */ 

	while( last!=NULL && last->next != NULL ) {
		last = (NORM_DATA_ITEM *)last->next;
	}

	/* Create the new item */
	newItem = (NORM_DATA_ITEM*)calloc(1,sizeof(NORM_DATA_ITEM));
	newItem->type = NORM_TYPE_RANGE;
	newItem->targetHigh = high;
	newItem->targetLow = low;
	newItem->next = NULL;

	/* Link the new item */
	if( last==NULL ) {
		/* Are we adding the first item */
		norm->firstItem = newItem;
	} else {
		/* Link to the end of the chain */
		last->next = (NORM_DATA_ITEM *)newItem;
	}
}

void NormDefReciprocal(NORM_DATA *norm) {
	NORM_DATA_ITEM *last = norm->firstItem;
	NORM_DATA_ITEM *newItem;

	/* Find the last item */ 

	while( last!=NULL && last->next != NULL ) {
		last = (NORM_DATA_ITEM *)last->next;
	}

	/* Create the new item */
	newItem = (NORM_DATA_ITEM*)calloc(1,sizeof(NORM_DATA_ITEM));
	newItem->type = NORM_TYPE_RECIPROCAL;
	newItem->next = NULL;

	/* Link the new item */
	if( last==NULL ) {
		/* Are we adding the first item */
		norm->firstItem = newItem;
	} else {
		/* Link to the end of the chain */
		last->next = (NORM_DATA_ITEM *)newItem;
	}
}

void NormDefClass(NORM_DATA *norm, int type, double low, double high) {
	NORM_DATA_ITEM *last = norm->firstItem;
	NORM_DATA_ITEM *newItem;

	/* Find the last item */ 

	while( last!=NULL && last->next != NULL ) {
		last = (NORM_DATA_ITEM *)last->next;
	}

	/* Create the new item */
	newItem = (NORM_DATA_ITEM*)calloc(1,sizeof(NORM_DATA_ITEM));
	newItem->type = type;
	newItem->targetHigh = high;
	newItem->targetLow = low;
	newItem->next = NULL;

	/* Link the new item */
	if( last==NULL ) {
		/* Are we adding the first item */
		norm->firstItem = newItem;
	} else {
		/* Link to the end of the chain */
		last->next = (NORM_DATA_ITEM *)newItem;
	}
}

void NormAnalyze(NORM_DATA *norm, char *filename) {
	FILE *fp;
	struct csv_parser p;
	char buf[1024];
	size_t bytes_read;

	// first, analyze the file
	if (csv_init(&p, CSV_APPEND_NULL) != 0) exit(EXIT_FAILURE);
	fp = fopen(filename, "rb");
	if (!fp)
	{ 
		printf("Could not open: %s\n", filename);
		exit(EXIT_FAILURE); 
	}

	norm->_currentColumn = 0;

	/* Read the file */
	while ((bytes_read=fread(buf, 1, 1024, fp)) > 0)
		if (csv_parse(&p, buf, bytes_read, _AnalyzeCallbackColumn, _AnalyzeCallbackRow, norm) != bytes_read) {
			fprintf(stderr, "Error while parsing file: %s\n",
			csv_strerror(csv_error(&p)) );
			exit(EXIT_FAILURE);
		}

	/* Handle any final data.  May call the callbacks once more */
	csv_fini(&p, _AnalyzeCallbackColumn, _AnalyzeCallbackRow, norm);

	/* Cleanup */
	fclose(fp);
	csv_free(&p);

	/* Decrease row count by one due to header */
	norm->rowCount--;
}

int NormCalculateActualCount(NORM_DATA *norm,int start, int size) {
	NORM_DATA_ITEM *item;
	int result;
	int columnIndex;

	item = norm->firstItem;

	result = 0;
	columnIndex = 0;

	while(item!=NULL) {
		if(columnIndex>=start && columnIndex<(start+size)) {
			switch(item->type) {
			case NORM_TYPE_PASS:
				result+=1;
				break;
			case NORM_TYPE_RANGE:
				result+=1;
				break;
			case NORM_CLASS_EQUILATERAL:
				result+=item->classCount-1;
				break;
			case NORM_CLASS_ONEOFN:
				result+=item->classCount;
				break;
			case NORM_TYPE_IGNORE:
				break;
			case NORM_TYPE_REPLACE:
				result+=1;
				break;
			}
		}

		if( item->type!=NORM_TYPE_IGNORE ) {
			columnIndex++;
		}


		item=item->next;
	}

	return result;
}



DATA_SET *NormProcess(NORM_DATA *norm, char *filename, int inputCount, int outputCount) {
	FILE *fp;
	struct csv_parser p;
	char buf[1024];
	size_t bytes_read;
	DATA_SET *result = NULL;
	_PROCESS_PAIR pair;
	int allocSize;

	_InitEquilateral(norm);

	/* Allocate the data set */
	result = (DATA_SET*)calloc(1,sizeof(DATA_SET));
    result->inputCount = NormCalculateActualCount(norm,0,inputCount);
    result->idealCount = NormCalculateActualCount(norm,inputCount,outputCount);
	result->recordCount = norm->rowCount;
	allocSize = norm->rowCount*(result->inputCount+result->idealCount);
	result->data = (double*)calloc(allocSize,sizeof(double));
    result->cursor = result->data;

	/* Construct the process_pair, to pass to callbacks */
	pair.norm = norm;
	pair.data = result;
	pair.rowCount = 0;

	/* Read and normalize file */
	if (csv_init(&p, CSV_APPEND_NULL) != 0) exit(EXIT_FAILURE);
	fp = fopen(filename, "rb");
	if (!fp)
	{ 
		printf("Could not open: %s\n", filename);
		exit(EXIT_FAILURE); 
	}

	norm->_currentColumn = 0;

	/* Read the file */
	while ((bytes_read=fread(buf, 1, 1024, fp)) > 0)
		if (csv_parse(&p, buf, bytes_read, _ProcessCallbackColumn, _ProcessCallbackRow, &pair) != bytes_read) {
			fprintf(stderr, "Error while parsing file: %s\n",
			csv_strerror(csv_error(&p)) );
			exit(EXIT_FAILURE);
		}

	/* Handle any final data.  May call the callbacks once more */
	csv_fini(&p, _ProcessCallbackColumn, _ProcessCallbackRow, &pair);

	/* Cleanup */
	fclose(fp);
	csv_free(&p);

	/* return the data set */
	return result;
}

NORM_DATA_ITEM *NormGetColumnItem(NORM_DATA *norm, int colIndex) {
	NORM_DATA_ITEM *result;

	result = norm->firstItem;

	while(colIndex>0) {
		if( result!=NULL ) {
			result = result->next;
		}
		colIndex--;
	}

	return result;
}