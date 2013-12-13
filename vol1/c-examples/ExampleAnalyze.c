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
This example performs a very basic analysis of the Iris data set.  The output is shown
here.


Column: "sepal_length",actualMin=7.90,actualHigh=4.30
Column: "sepal_width",actualMin=4.40,actualHigh=2.00
Column: "petal_length",actualMin=6.90,actualHigh=1.00
Column: "petal_width",actualMin=2.50,actualHigh=0.10
Column: "class",classes= "Iris-setosa";"Iris-versicolor";"Iris-virginica";
Rows: 150
*/
void ExampleAnalyze(int argIndex, int argc, char **argv) {	
	char filename[FILENAME_MAX];
	NORM_DATA *norm;
	NORM_DATA_ITEM *col;
	NORM_DATA_CLASS *currentClass;

	LocateFile("iris.csv",filename,FILENAME_MAX);
	
	/* Setup normalization for the Iris data set.  Note, if you want to use this example
	   for other data sets, this section will need to change. */
	norm = NormCreate();
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefRange(norm,0,1);
	NormDefClass(norm,NORM_CLASS_ONEOFN,0,1);
	/* Analyze the file */
	NormAnalyze(norm,filename);
	
	/* Loop and display stats on all columns. */
	col = norm->firstItem;
	while(col!=NULL) {
		/* For a simple continuous field, just display the range. */
		if( col->type == NORM_TYPE_RANGE ) {
			printf("Column: \"%s\",actualMin=%.2f,actualHigh=%.2f\n",col->name,col->actualHigh,col->actualLow);
		} else {
		/* For a discrete field, display the classes. */
			printf("Column: \"%s\",classes= ",col->name);
			currentClass = col->firstClass;
			while(currentClass!=NULL) {
				printf("\"%s\";",currentClass->name);
				currentClass = currentClass->next;
			}
			printf("\n");

		}
		col = col->next;
	}

	/* Display rows in file */
	printf("Rows: %i\n",norm->rowCount);

	/* Cleanup */
	NormDelete(norm);
}