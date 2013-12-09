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

#include "aifh-vol1.h"

void ExampleTest(int argIndex, int argc, char **argv);

/* Chapter 1 */
void ExampleReadCSV(int argIndex, int argc, char **argv);

/* Chapter 2 */
void ExampleSimpleNormalize(int argIndex, int argc, char **argv);
void ExampleAnalyze(int argIndex, int argc, char **argv);
void ExampleNormalize(int argIndex, int argc, char **argv);

/* Chapter 3 */
void ExampleDistance(int argIndex, int argc, char **argv);

/* Chapter 4 */
void ExamplePI(int argIndex, int argc, char **argv);
void ExampleRandom(int argIndex, int argc, char **argv);

/* Chapter 5 */
void ExampleKMeans(int argIndex, int argc, char **argv);

/* Chapter 6 */
void ExampleError(int argIndex, int argc, char **argv);

/* Chapter 7 */
void ExamplePoly(int argIndex, int argc, char **argv);
void ExampleRandXOR(int argIndex, int argc, char **argv);
void ExampleRandIris(int argIndex, int argc, char **argv);

/* Chapter 8 */
void ExampleNelderMeadIris(int argIndex, int argc, char **argv);
void ExampleHillClimbIris(int argIndex, int argc, char **argv);
void ExampleAnnealIris(int argIndex, int argc, char **argv);

/* Chapter 9 */
void ExampleAnnealTSP(int argIndex, int argc, char **argv);
void ExampleAnnealKnapsack(int argIndex, int argc, char **argv);

/* Chapter 10 */
void ExampleLinearRegression(int argIndex, int argc, char **argv);
void ExampleGLM(int argIndex, int argc, char **argv);

/* Utility */
void LocateFile(char *filename, char *resultName, size_t size);

#ifdef __cplusplus
}
#endif

#endif