#include "aifh-vol1-examples.h"

int main(int argc, char* argv[])
{
	int i;
	int pause = 0;
	int success = 0;

	for(i=1;i<argc;i++) {
		if( *argv[i]=='-' ) {
			if( _strcmpi(argv[i],"-pause") == 0) {
				pause = 1;
			} else {
				printf("Unknown option: %s",argv[i]);
			}
		}
		else {
			if( _strcmpi(argv[i],"readcsv") == 0 ) {
				success = 1;
				ExampleReadCSV(i+1,argc,argv);
			}
		}
	}

	if( !success ) {
		printf("Usage:\n%s [-pause] ExampleName [arg1] [args2] ...\n",argv[0]);
		printf("\nWhere ExampleName is one of:\n");
		printf("**Chapter 1**\n");
		printf("readcsv - Read the contents of a CSV file\n");
	}

	if( pause ) {
		printf("[Press Any Key to Exit]");
		getchar();
	}

	return 0;
}

