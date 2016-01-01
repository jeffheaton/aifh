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
This example uses linear regression to predict the number of rings from the abalone data set.  Linear regression
requires a simple linear relationship so it will not fit exactly, however, it is able to find some trends.
The output is shown here.

Solution coefficients: [[ -3.70992979e+12]
 [  3.70992979e+12]
 [  3.70992979e+12]
 [  3.70992979e+12]
 [ -4.55322266e-01]
 [  1.10373535e+01]
 [  1.07808838e+01]
 [  8.97070312e+00]
 [ -1.97780762e+01]
 [ -1.05693359e+01]
 [  8.75000000e+00]]
 -> Actual: [ 9.21191406], Ideal:15.0
 -> Actual: [ 7.84033203], Ideal:7.0
 -> Actual: [ 11.15673828], Ideal:9.0
 -> Actual: [ 9.64404297], Ideal:10.0
 -> Actual: [ 6.74414062], Ideal:7.0
 -> Actual: [ 7.82226562], Ideal:8.0
...
 -> Actual: [ 9.58837891], Ideal:8.0
 -> Actual: [ 9.10546875], Ideal:10.0
 -> Actual: [ 11.17578125], Ideal:10.0
 -> Actual: [ 10.16894531], Ideal:8.0
 -> Actual: [ 10.74511719], Ideal:11.0
 -> Actual: [ 9.98828125], Ideal:10.0
 -> Actual: [ 10.94140625], Ideal:9.0
 -> Actual: [ 9.79785156], Ideal:10.0
 -> Actual: [ 10.98486328], Ideal:12.0

Process finished with exit code 0

"""
__author__ = 'jheaton'

import os
import sys
import numpy as np

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from normalize import Normalize


def multi_linear_regression(x, y):
    x_matrix = np.ones((len(x), len(x[0]) + 1), dtype=float)
    x_matrix[:, 1:] = x
    print(y)
    return np.linalg.lstsq(x_matrix, y)[0]


def calc_linear_regression(coeff, x):
    result = 0
    for i in range(1, len(coeff)):
        result += x[i - 1] * coeff[i]

    result += coeff[0]
    return result


# find the Iris data set
abaloneFile = os.path.dirname(os.path.realpath(__file__))
abaloneFile = os.path.abspath(abaloneFile + "../../datasets/abalone.csv")

# Normalize abalone file.

norm = Normalize()
abalone_work = norm.load_csv(abaloneFile)

# Make all columns beyond col #1 numeric.
for i in range(1, 9):
    norm.make_col_numeric(abalone_work, i)

# Discover all of the classes for column #1, the gender.
classes = norm.build_class_map(abalone_work, 0)

# Normalize gender one-of-n encoding.
norm.norm_col_one_of_n(abalone_work, 0, classes, 0, 1)

# Separate into input and ideal.

training = np.array(abalone_work)
training_input = training[:, 0:10]
training_ideal = training[:, 10:11]

coeff = multi_linear_regression(training_input, training_ideal)

print("Solution coefficients: " + str(coeff))

# Evaluate.
for i in range(0, len(training_input)):
    row = training_input[i]
    y = calc_linear_regression(coeff, row)
    print( " -> Actual: " + str(y) + ", Ideal:" + str(training_ideal[i][0]))