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
This example uses a greedy random trainer to fit the coefficients of a 3rd degree polynomial.  Because it is totally
random it takes 100k iterations, and can run for awhile.  The output is shown below.

The ideal coefficients would be:
6+4x+2x^2

However, the final output is reasonably close:

-3.95186312458+4.06501034511x+2.0113298397x^2


Iteration #99963, Score: 7397.2097385
Iteration #99964, Score: 7397.2097385
Iteration #99965, Score: 7397.2097385
Iteration #99966, Score: 7397.2097385
Iteration #99967, Score: 7397.2097385
Iteration #99968, Score: 7397.2097385
Iteration #99969, Score: 7397.2097385
Iteration #99970, Score: 7397.2097385
Iteration #99971, Score: 7397.2097385
Iteration #99972, Score: 7397.2097385
Iteration #99973, Score: 7397.2097385
Iteration #99974, Score: 7397.2097385
Iteration #99975, Score: 7397.2097385
Iteration #99976, Score: 7397.2097385
Iteration #99977, Score: 7397.2097385
Iteration #99978, Score: 7397.2097385
Iteration #99979, Score: 7397.2097385
Iteration #99980, Score: 7397.2097385
Iteration #99981, Score: 7397.2097385
Iteration #99982, Score: 7397.2097385
Iteration #99983, Score: 7397.2097385
Iteration #99984, Score: 7397.2097385
Iteration #99985, Score: 7397.2097385
Iteration #99986, Score: 7397.2097385
Iteration #99987, Score: 7397.2097385
Iteration #99988, Score: 7397.2097385
Iteration #99989, Score: 7397.2097385
Iteration #99990, Score: 7397.2097385
Iteration #99991, Score: 7397.2097385
Iteration #99992, Score: 7397.2097385
Iteration #99993, Score: 7397.2097385
Iteration #99994, Score: 7397.2097385
Iteration #99995, Score: 7397.2097385
Iteration #99996, Score: 7397.2097385
Iteration #99997, Score: 7397.2097385
Iteration #99998, Score: 7397.2097385
Iteration #99999, Score: 7397.2097385
Iteration #100000, Score: 7397.2097385
Finished after 100001 iterations, final score is 7397.2097385
Final polynomial
-3.95186312458+4.06501034511x+2.0113298397x^2

"""
__author__ = 'jheaton'

import os
import sys

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

import numpy as np
from train import TrainGreedRandom
from error import ErrorCalculation


def poly(coeff, x):
    """
    Polynomial function.  Calculate y given the specified x and coefficient values.
    @param coeff: The coefficients to use.
    @param x: The x values to use.
    @return: The function result y,
    """
    return [(coeff[2] * (x ** 2)) + (coeff[1] * x) + coeff[0]]


def build_training_set():
    """
    Build a training set for the polynomial.  Generate between -50 and 50.
    @return: The training set.
    """
    result_input = []
    result_ideal = []
    coeff = [6, 4, 2]

    for x in range(-50, 50):
        y = poly(coeff, x)
        result_input.append([x])
        result_ideal.append(y)

    return result_input, result_ideal


def print_poly(coeff):
    """
    Display The polynomial formula.
    @param coeff: The coefficients.
    """
    result = ""

    for i in range(0, len(coeff)):
        c = coeff[i]

        if len(result) > 0:
            if c >= 0:
                result += "+"

        result += str(c)

        if i >= 2:
            result += "x^"
            result += str(i)
        elif i >= 1:
            result += "x"
    print(result)


def score_funct(coeff):
    """
    Calculate the score with the specified coefficients.  Use MSE error calculation.
    @param coeff: The coefficients.
    @return: The score.  We are trying to minimize this score.
    """
    global input_data
    global output_data

    # Calculate the actual output of the polynomial with the specified coefficients.
    actual_output = []
    for input_data in training_input:
        x = input_data[0]
        output_data = poly(coeff, x)
        actual_output.append(output_data)
    return ErrorCalculation.sse(np.array(actual_output), training_ideal)

# Build the training set.
training_input, training_ideal = build_training_set()

# Extract the input and ideal training.
training_input = np.array(training_input)
training_ideal = np.array(training_ideal)

# Starting point for coefficients.
x0 = [0, 0, 0]

# Perform the train.
train = TrainGreedRandom(-10, 10)
train.stop_score = 100
train.display_iteration = True
result = train.train(x0, score_funct)

# Evaluate the polynomial.
print("Final polynomial")
print_poly(result)