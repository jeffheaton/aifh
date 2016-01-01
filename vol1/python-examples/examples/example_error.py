#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 1: Fundamental Algorithms
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com

    Code repository:
    https://github.com/jeffheaton/aifh

    Copyright 2013 by Jeff Heaton

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

    For more information on Heaton Research copyrights, licenses
    and trademarks visit:
    http://www.heatonresearch.com/copyright
============================================================================================================
This example shows how three different error metrics calculate different data sets.

Type	SSE			MSE		RMS
Small	2505		0.01	0.10
Medium	62628		0.25	0.50
Large	250515		1.00	1.00
Huge	25051524	100.21	10.01
"""
__author__ = 'jheaton'

import os
import sys
import numpy as np

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from error import ErrorCalculation

# Operating parameters.
SEED = 1420
ROWS = 10000
COLS = 25
LOW = -1
HIGH = 1

# Generate ideal and actual values.

def generate(seed, rows, cols, low, high, distort):
    """
    Generate random data, this includes a random ideal output and a distorted actual output.  This simulates an a
    actual model failing to exactly predict the ideal data by the specified distortion factor.
    @param seed: The seed to use.
    @param rows: The number of rows to generate.
    @param cols: The number of columns to generate.
    @param low: The low value of the random numbers.
    @param high: The high values of the random numbers.
    @param distort: The amount to distort by.
    @return: A dictionary that contains the actual and ideal outputs.
    """
    result = {}

    np.random.seed(seed)

    ideal = np.zeros((rows, cols), dtype=float)
    actual = np.zeros((rows, cols), dtype=float)

    result['ideal'] = ideal
    result['actual'] = actual

    for row in range(0, rows):
        for col in range(0, cols):
            d = float(np.random.randint(low, high))
            ideal[row][col] = d
            actual[row][col] = d + (np.random.normal() * distort)

    return result

# Generate data sets.
smallErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 0.1)
mediumErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 0.5)
largeErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 1.0)
hugeErrors = generate(SEED, ROWS, COLS, LOW, HIGH, 10.0)

small_sse = ErrorCalculation.sse(smallErrors['actual'], smallErrors['ideal'])
small_mse = ErrorCalculation.mse(smallErrors['actual'], smallErrors['ideal'])
small_rms = ErrorCalculation.rms(smallErrors['actual'], smallErrors['ideal'])

medium_sse = ErrorCalculation.sse(mediumErrors['actual'], mediumErrors['ideal'])
medium_mse = ErrorCalculation.mse(mediumErrors['actual'], mediumErrors['ideal'])
medium_rms = ErrorCalculation.rms(mediumErrors['actual'], mediumErrors['ideal'])

large_sse = ErrorCalculation.sse(largeErrors['actual'], largeErrors['ideal'])
large_mse = ErrorCalculation.mse(largeErrors['actual'], largeErrors['ideal'])
large_rms = ErrorCalculation.rms(largeErrors['actual'], largeErrors['ideal'])

huge_sse = ErrorCalculation.sse(hugeErrors['actual'], hugeErrors['ideal'])
huge_mse = ErrorCalculation.mse(hugeErrors['actual'], hugeErrors['ideal'])
huge_rms = ErrorCalculation.rms(hugeErrors['actual'], hugeErrors['ideal'])

print("Type\tSSE\t\t\tMSE\t\tRMS")
print("Small\t" + str(int(small_sse)) + "\t\t" + "{0:.2f}".format(small_mse) + "\t" + "{0:.2f}".format(small_rms))
print("Medium\t" + str(int(medium_sse)) + "\t\t" + "{0:.2f}".format(medium_mse) + "\t" + "{0:.2f}".format(medium_rms))
print("Large\t" + str(int(large_sse)) + "\t\t" + "{0:.2f}".format(large_mse) + "\t" + "{0:.2f}".format(large_rms))
print("Huge\t" + str(int(huge_sse)) + "\t" + "{0:.2f}".format(huge_mse) + "\t" + "{0:.2f}".format(huge_rms))
