#include "aifh-vol1.h"

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

		if( access( resultName, F_OK ) != -1 ) {
			done=1;
		} else if( depth>10 ) {
			done=1;
		}

		depth++;
	}
}
