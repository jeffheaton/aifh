#include "aifh-vol1.h"

static void _NormCallbackColumn (void *s, size_t len, void *data) 
{
	NORM_DATA *norm;

	norm = (NORM_DATA *)data;
}

static void _NormCallbackRow (int c, void *data) 
{
	NORM_DATA *norm;

	norm = (NORM_DATA *)data;
	norm->rowCount++;
}

NORM_DATA *NormCreate() {
	NORM_DATA *result = (NORM_DATA *)calloc(1,sizeof(NORM_DATA));
	return result;
}

void NormDelete(NORM_DATA *norm) {
	NORM_DATA_ITEM *item, *t;
	item = norm->firstItem;
	while(item!=NULL) {
		t = (NORM_DATA_ITEM *)item->next;
		free(item);
		item = t;
	}

	free(norm);

}

void NormDefRange(NORM_DATA *norm, double low, double high) {
	NORM_DATA_ITEM *last = norm->firstItem;
	NORM_DATA_ITEM *newItem;

	/* Find the last item */ 

	while( last->next != NULL ) {
		last = (NORM_DATA_ITEM *)last->next;
	}

	/* Create the new item */
	newItem = (NORM_DATA_ITEM*)calloc(1,sizeof(NORM_DATA_ITEM));
	newItem->targetHigh = high;
	newItem->targetLow = low;
	newItem->next = NULL;

	/* Link the new item */
	last->next = (NORM_DATA_ITEM *)newItem;
}

void NormProcess(NORM_DATA *norm, char *filename, int inputCount, int outputCount) {
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

	while ((bytes_read=fread(buf, 1, 1024, fp)) > 0)
		if (csv_parse(&p, buf, bytes_read, _NormCallbackColumn, _NormCallbackRow, norm) != bytes_read) {
			fprintf(stderr, "Error while parsing file: %s\n",
			csv_strerror(csv_error(&p)) );
			exit(EXIT_FAILURE);
		}

	/* Handle any final data.  May call the callbacks once more */
	csv_fini(&p, _NormCallbackColumn, _NormCallbackRow, norm);

	/* Cleanup */
	fclose(fp);
	csv_free(&p);

	// now actually normalize

}