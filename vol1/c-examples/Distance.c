#include "aifh-vol1.h"

#include <math.h>

double DistanceEuclidean(
	double *position1, 
	int pos1, 
	double *position2, 
	int pos2, 
	int length) {
        double sum = 0,d;
		int i;
        
		for (i = 0; i < length; i++) {
            d = position1[i + pos1] - position2[i + pos1];
            sum += d * d;
        }
        return sqrt(sum);
    }

double DistanceManhattan(
	double *position1, 
	int pos1, 
	double *position2, 
	int pos2, 
	int length) {
        double sum = 0, d;
		int i;

        for (i = 0; i < length; i++) {
            d = fabs(position1[i] - position2[i]);
            sum += d;
        }
        return sum;
    }

double DistanceChebyshev(
	double *position1, 
	int pos1, 
	double *position2, 
	int pos2, 
	int length) {
        double result = 0, d;
		int i;

        for (i = 0; i < length; i++) {
            d = fabs(position1[pos1 + i] - position2[pos2 + i]);
            result = MAX(d, result);
        }
        return result;
    }