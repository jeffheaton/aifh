#include "aifh-vol1-examples.h"

void ExampleDistance(int argIndex, int argc, char **argv) {
	double pos1[3] = { 1.0, 2.0, 3.0 };
	double pos2[3] = { 4.0, 5.0, 6.0 };
	double pos3[3] = { 7.0, 8.0, 9.0 };

	printf("Euclidean Distance\n");
	printf("pos1->pos2: %.2f\n", DistanceEuclidean(pos1,0,pos2,0,3));
	printf("pos2->pos3: %.2f\n", DistanceEuclidean(pos2,0,pos3,0,3));
	printf("pos3->pos1: %.2f\n", DistanceEuclidean(pos3,0,pos1,0,3));
	printf("\nManhattan Distance\n");
	printf("pos1->pos2: %.2f\n", DistanceManhattan(pos1,0,pos2,0,3));
	printf("pos2->pos3: %.2f\n", DistanceManhattan(pos2,0,pos3,0,3));
	printf("pos3->pos1: %.2f\n", DistanceManhattan(pos3,0,pos1,0,3));
	printf("\nChebyshev Distance\n");
	printf("pos1->pos2: %.2f\n", DistanceChebyshev(pos1,0,pos2,0,3));
	printf("pos2->pos3: %.2f\n", DistanceChebyshev(pos2,0,pos3,0,3));
	printf("pos3->pos1: %.2f\n", DistanceChebyshev(pos3,0,pos1,0,3));

}