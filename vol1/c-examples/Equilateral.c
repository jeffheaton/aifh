#include "aifh-vol1.h"

void equilat (
   int classCount,
   double *outputMatrix
   )
{
	int i, j, k, rowSize ;
	double r, f ;

	rowSize = classCount - 1 ;
	outputMatrix[0] = -1.0 ;
	outputMatrix[rowSize] = 1.0 ;

	for (k=2 ; k<classCount ; k++) {
		r = (double) k;
		f = sqrt ( r * r - 1.0 ) / r ; 
		for (i=0 ; i<k ; i++) {        
			for (j=0 ; j<k-1 ; j++) {     
				outputMatrix[i*rowSize+j] *= f ;
			}

			r = -1.0 / r ;
			for (i=0 ; i<k ; i++) {
				outputMatrix[i*rowSize+k-1] = r ;
			}

			for (i=0 ; i<k-1 ; i++) {
				outputMatrix[k*rowSize+i] = 0.0 ;
			}

			outputMatrix[k*rowSize+k-1] = 1.0 ;
		}
	}
}
