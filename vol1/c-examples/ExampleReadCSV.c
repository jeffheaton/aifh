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


struct counts {
	long unsigned fields;
	long unsigned rows;
};

void CallbackColumn (void *s, size_t len, void *data) 
{
	printf("Field: \"%s\"\n",(char*)s);
	((struct counts *)data)->fields++; 
}
void CallbackRow (int c, void *data) 
{
	printf("Row done\n");
	((struct counts *)data)->rows++; 
}

/*
This example shows how to simply read a CSV file.  This example reads the iris data set.  
The output is shown here.  

Reading CSV file: ./datasets/iris.csv
Field: "sepal_length"
Field: "sepal_width"
Field: "petal_length"
Field: "petal_width"
Field: "class"
Row done
Field: "5.1"
Field: "3.5"
Field: "1.4"
Field: "0.2"
Field: "Iris-setosa"
Row done
Field: "4.9"
Field: "3.0"
Field: "1.4"
Field: "0.2"
Field: "Iris-setosa"
Row done
Field: "4.7"
Field: "3.2"
Field: "1.3"
Field: "0.2"
Field: "Iris-setosa"
...
Row done
Field: "6.5"
Field: "3.0"
Field: "5.2"
Field: "2.0"
Field: "Iris-virginica"
Row done
Field: "6.2"
Field: "3.4"
Field: "5.4"
Field: "2.3"
Field: "Iris-virginica"
Row done
Field: "5.9"
Field: "3.0"
Field: "5.1"
Field: "1.8"
Field: "Iris-virginica"
Row done
755 fields, 151 rows
*/
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