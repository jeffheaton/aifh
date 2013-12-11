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

DATA_SET *DataCreate(int rowCount, int inputCount, int outputCount) {
	DATA_SET *result = NULL;
	int allocSize;

	/* Allocate the data set */
	result = (DATA_SET*)calloc(1,sizeof(DATA_SET));
    result->inputCount = inputCount;
    result->idealCount = outputCount;
	result->recordCount = rowCount;
	allocSize = rowCount*(result->inputCount+result->idealCount);
	result->data = (double*)calloc(allocSize,sizeof(double));
    result->cursor = result->data;

	return result;
}

void DataDelete(DATA_SET *data) {
	free(data);
}

double *DataGetInput(DATA_SET *data, unsigned int index)
{
    int i;
	i = index*(data->inputCount+data->idealCount);
    return &data->data[i];
}

double *DataGetIdeal(DATA_SET *data, unsigned int index)
{
    int i;
	i = index*(data->inputCount+data->idealCount);
    return &data->data[i+data->inputCount];
}

void DataCSVSave(FILE *fp,NORM_DATA *norm, DATA_SET *data)
{
    unsigned int i,j,len;
    double *input, *ideal;
    NORM_DATA_ITEM *item;

	/* Write the header */
	if( norm!=NULL ) {
		item = norm->firstItem;
		j=0;
		while(item!=NULL) {
			/* Determine the length of the normalized column */
			len=NormCalculateActualCount(norm,j,1);

			if( len==1 ) {
				/* Length 1, just append a normal column head */
				if( j>0 ) {
					fprintf(fp,",");
				}
				fprintf(fp,"\"%s\"",item->name);
				j++;
			} else {
				/* Otherwise, append the correct number of columns it normalizes into */
				for(i=0;i<len;i++) {
					if( j>0 ) {
						fprintf(fp,",");
					}
					fprintf(fp,"\"%s-%i\"",item->name,i);
					j++;
				}
			}
			item = item->next;
			j++;
		}
		fprintf(fp,"\n");
	}

	/* Write the data */
    for(i=0; i<data->recordCount; i++)
    {
        input = DataGetInput(data,i);
        ideal = DataGetIdeal(data,i);

        for(j=0; j<data->inputCount; j++)
        {
            if(j>0)
            {
                fprintf(fp,",");
            }
 			fprintf(fp,"%f",input[j]);
        }

        for(j=0; j<data->idealCount; j++)
        {
            fprintf(fp,",");
            fprintf(fp,"%f",ideal[j]);
        }
        fputs("\n",fp);
    }
    fclose(fp);
}

void DataMoveCursor(DATA_SET *data, int location) {
	data->cursor = data->data + (location * (data->inputCount+data->idealCount));
}

void DataAddVar(DATA_SET *data, ...)
{
    int i,total;
    double d = 0.0;
	va_list arguments;

    va_start ( arguments, data );
    total = data->inputCount + data->idealCount;

    for(i=0; i<total; i++)
    {
        d = (double)va_arg(arguments,double);
        *(data->cursor++) = d;
    }

    va_end( arguments );
}
