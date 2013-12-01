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
from rbf_network import RbfNetwork
from error import ErrorCalculation
from train import TrainAnneal

# find the Iris data set
irisFile = os.path.dirname(os.path.realpath(__file__))
irisFile = os.path.abspath(irisFile + "../../datasets/iris.csv")

# Read the Iris data set.
print('Reading CSV file: ' + irisFile)
norm = Normalize()
iris_work = norm.load_csv(irisFile)

# Extract the original iris species so we can display during the final validation.
ideal_species = [row[4] for row in iris_work]

# Setup the first four fields to "range normalize" between -1 and 1.
for i in range(0, 4):
    norm.make_col_numeric(iris_work, i)
    norm.norm_col_range(iris_work, i, 0, 1)

# Discover all of the classes for column #4, the iris species.
classes = norm.build_class_map(iris_work, 4)
inv_classes = {v: k for k, v in classes.items()}

# Normalize iris species using one-of-n.
# We could have used equilateral as well.  For an example of equilateral, see the example_nm_iris example.
norm.norm_col_one_of_n(iris_work, 4, classes, 0, 1)


# Prepare training data.  Separate into input and ideal.
training = np.array(iris_work)
training_input = training[:, 0:4]
training_ideal = training[:, 4:7]

# Create an RBF network.  There are four inputs and two outputs.
# There are also five RBF functions used internally.
# You can experiment with different numbers of internal RBF functions.
# However, the input and output must match the data set.
network = RbfNetwork(4, 4, 3)
network.reset()

best_score = sys.float_info.max


def score_funct(x):
    """
    The score function for Iris anneal.
    @param x:
    @return:
    """
    global best_score
    global input_data
    global output_data
    # Update the network's long term memory to the vector we need to score.
    network.copy_memory(x)
    # Loop over the training set and calculate the output for each.
    actual_output = []
    for input_data in training_input:
        output_data = network.compute_regression(input_data)
        actual_output.append(output_data)
    # Calculate the error with MSE.
    result = ErrorCalculation.mse(np.array(actual_output), training_ideal)
    # If we improved the score, then report it.
    if result < best_score:
        best_score = result
        print("Score: " + str(result))
    return result


x0 = network.longTermMemory[:]

# Perform the annealing
train = TrainAnneal()
train.display_iteration = True
train.train(x0, score_funct)

# Display the final validation.  We show all of the iris data as well as the predicted species.
for i in xrange(0, len(training_input)):
    input_data = training_input[i]
    # Compute the output from the RBF network
    output_data = network.compute_regression(input_data)
    ideal_data = training_ideal[i]
    # Decode the three output neurons into a class number.
    class_id = norm.denorm_one_of_n(output_data)
    print(str(input_data) + " -> " + inv_classes[class_id] + ", Ideal: " + ideal_species[i])