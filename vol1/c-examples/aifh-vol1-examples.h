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
#ifndef __AIFH_VOL1_EXAMPLES_H
#define __AIFH_VOL1_EXAMPLES_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>	
#include <errno.h>


#include "csv.h"

/* Chapter 1 Examples */
void ExampleReadCSV(int argIndex, int argc, char **argv);

/* For visual C++ */
#ifdef _MSC_VER
#include <io.h>
#define strcasecmp _strcmpi
#pragma warning(disable : 4996)
#define F_OK    0       /* Test for existence.  */
#else
/* For non-Visual C++ */
#include <unistd.h>
#endif

#ifdef __cplusplus
}
#endif

#endif