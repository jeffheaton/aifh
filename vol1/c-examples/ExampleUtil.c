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

void LocateFile(char *filename, char *resultName, size_t size) {
	int depth = 0;
	int done = 0;
	int i;

	/* Does it exist in the current directory? */
	strncpy(resultName,"./datasets/",size);
	strncat(resultName,filename,size);
	if( access( resultName, F_OK ) != -1 ) {
		return;
	}

	/* Search parent directories */
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
