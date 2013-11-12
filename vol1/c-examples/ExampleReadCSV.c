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

void LocateFile(char *filename, char *resultName, size_t size) {
	int depth = 0;
	int done = 0;
	int i;

	while(!done) {
		strncpy(resultName,"./",size);

		for(i=0;i<depth;i++) {
			strncat(resultName,"../",size);
		}

		if( depth>0 ) {
			strncat(resultName,"datasets/",size);
		}

		strncat(resultName,filename,size);

		printf("%s\n", resultName);

		if( access( resultName, F_OK ) != -1 ) {
			done=1;
		} else if( depth>10 ) {
			done=1;
		}

		depth++;
	}
}

struct counts {
	long unsigned fields;
	long unsigned rows;
};

void CallbackColumn (void *s, size_t len, void *data) 
{
	size_t i;
	for(i=0;i<len;i++) {
		printf("Field: \"%s\"\n",s);
	}
	((struct counts *)data)->fields++; 
}
void CallbackRow (int c, void *data) 
{
	printf("Row done\n");
	((struct counts *)data)->rows++; 
}


void ExampleReadCSV(int argIndex, int argc, char **argv) {
	char filename[FILENAME_MAX];
	FILE *fp;
	struct csv_parser p;
	char buf[1024];
	size_t bytes_read;
	struct counts c = {0, 0};
	
	if( argIndex>=argc ) {
		LocateFile("iris.csv",filename,FILENAME_MAX);
	} else {
		strncpy(filename,argv[argIndex],FILENAME_MAX);
	}
	
	printf("Reading CSV file: %s\n", filename);

	/* Setup csvlib to read the CSV file */
	if (csv_init(&p, CSV_APPEND_NULL) != 0) exit(EXIT_FAILURE);
	fp = fopen(filename, "rb");
	if (!fp)
	{ 
		printf("Could not open: %s\n", filename);
		exit(EXIT_FAILURE); 
	}

	/* Loop over the contents.  It is important to note that we are not reading line by
	   line, at this level.  Rather, we are passing blocks off to csvlib.  Then csvlib
	   calls our two callbacks as needed. */

	while ((bytes_read=fread(buf, 1, 1024, fp)) > 0)
		if (csv_parse(&p, buf, bytes_read, CallbackColumn, CallbackRow, &c) != bytes_read) {
			fprintf(stderr, "Error while parsing file: %s\n",
			csv_strerror(csv_error(&p)) );
			exit(EXIT_FAILURE);
		}

	/* Handle any final data.  May call the callbacks once more */
	csv_fini(&p, CallbackColumn, CallbackRow, &c);

	/* Print final stats on CSV file */
	printf("%lu fields, %lu rows\n", c.fields, c.rows);

	/* Cleanup */
	fclose(fp);
	csv_free(&p);
}