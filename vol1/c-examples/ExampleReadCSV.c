#include "aifh-vol1-examples.h"

void LocateFile() {
}

void ExampleReadCSV(int argIndex, int argc, char **argv) {
	if( argIndex>=argc ) {
		printf("No filename provided.\n");
	} else {
		printf("Reading CSV file: %s\n", argv[argIndex]);
	}
	printf("Ready to read a CSV!\n");
}