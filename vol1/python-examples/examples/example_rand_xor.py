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
This example makes use of a greedy random trainer to  to fit an RBF network to the XOR data set.  Because it is totally
random it takes 100k iterations, and can run for awhile.  The sample output below was also only able to train to 0.09.
The output is shown below.

...
Iteration #99981, Score: 0.098615887498
Iteration #99982, Score: 0.098615887498
Iteration #99983, Score: 0.098615887498
Iteration #99984, Score: 0.098615887498
Iteration #99985, Score: 0.098615887498
Iteration #99986, Score: 0.098615887498
Iteration #99987, Score: 0.098615887498
Iteration #99988, Score: 0.098615887498
Iteration #99989, Score: 0.098615887498
Iteration #99990, Score: 0.098615887498
Iteration #99991, Score: 0.098615887498
Iteration #99992, Score: 0.098615887498
Iteration #99993, Score: 0.098615887498
Iteration #99994, Score: 0.098615887498
Iteration #99995, Score: 0.098615887498
Iteration #99996, Score: 0.098615887498
Iteration #99997, Score: 0.098615887498
Iteration #99998, Score: 0.098615887498
Iteration #99999, Score: 0.098615887498
Iteration #100000, Score: 0.098615887498
Finished after 100001 iterations, final score is 0.098615887498
[ 0.  0.] -> [0.47032350248938742]
[ 1.  0.] -> [0.65767372384676726]
[ 0.  1.] -> [0.91362969255900606]
[ 1.  1.] -> [0.22048184425450046]
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
from train import TrainGreedRandom


# The input for the XOR operator.
training_input = np.array([
    [0.0, 0.0],
    [1.0, 0.0],
    [0.0, 1.0],
    [1.0, 1.0]])

# The ideal output for the XOR operator.  Each row corresponds to a row in the training_input.
training_ideal = np.array([
    [0.0],
    [1.0],
    [1.0],
    [0.0],
])

# Create the network.  2 inputs, 1 output and 5 rbf functions.
network = RbfNetwork(2, 5, 1)
network.reset()


def score_funct(x):
    """
    The score function.  Calculate the MSE error between the actual network output and the ideal values for the XOR.
    @param x: The long term memory that we are to score.
    @return: The MSE error.
    """
    # Setup the long-term memory that we would like to test.
    network.copy_memory(x)
    # Present all inputs to the network and accumulate the output for each.
    actual_output = []
    for input_data in training_input:
        output_data = network.compute_regression(input_data)
        actual_output.append(output_data)
    # Compare the actual output with the ideal expected output and calculate the MSE error.
    return ErrorCalculation.mse(np.array(actual_output), training_ideal)

# Use the initial long term memory of the network as the starting state.
x0 = list(network.long_term_memory)

# Train with greedy random.
train = TrainGreedRandom(-1, 1)
train.display_iteration = True
train.max_iterations = 100000
train.stop_score = 0.05
result = train.train(x0, score_funct)

# Copy the final trained long-term memory to the network so we can use it for evaluation.
network.copy_memory(result)

# Display the output for the XOR.  XOR will not be trained perfectly.  You should see that the (0,1) and (1,0) inputs
# are both close to 1.0, whereas the (1,1) and (0,0) are close to 0.0.
for input_data in training_input:
    output_data = network.compute_regression(input_data)
    print(str(input_data) + " -> " + str(output_data))