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
This example uses hill climbing to fit a RBF network to the XOR data set.  Hill climbing will give very mixed results.
My example below trained below 0.01 in two iterations!  This is because the initial random position of the
RBF network was pretty good.  Often you will need considerably more iterations for a considerably worse outcome.

It is also important to notice the output does not match the XOR exactly.  For most XOR problems, this will be the
case.  However, the output is close.  For [0,0] and [1,1] we are much closer to 0 output than [1,0] and [0,1].

Iteration #1, Score: 0.184437854972
Iteration #2, Score: 0.0140165091783
Finished after 3 iterations, final score is 0.0140165091783
[ 0.  0.] -> [0.018693711466584106]
[ 1.  0.] -> [0.96159220720236505]
[ 0.  1.] -> [1.0163212099901124]
[ 1.  1.] -> [0.23232529225575432]
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
from train import TrainHillClimb

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

network = RbfNetwork(2, 5, 1)
network.reset()


def score_funct(x):
    """
    The score function.  Calculate the MSE error between the actual network output and the ideal values for the XOR.
    @param x: The long term memory that we are to score.
    @return: The MSE error.
    """
    global input_data
    global output_data
    network.copy_memory(x)
    actual_output = []
    for input_data in training_input:
        output_data = network.compute_regression(input_data)
        actual_output.append(output_data)
    return ErrorCalculation.mse(np.array(actual_output), training_ideal)

# Create a copy of the long-term memory.  This becomes the initial state.
x0 = list(network.long_term_memory)

# Train with hill climbing.
train = TrainHillClimb()
train.display_iteration = True
train.max_iterations = 100
train.stop_score = 0.05
result = train.train(x0, score_funct)

# Copy our final state to the long term memory of the network.
network.copy_memory(result)

# Display the output for the XOR.  XOR will not be trained perfectly.  You should see that the (0,1) and (1,0) inputs
# are both close to 1.0, whereas the (1,1) and (0,0) are close to 0.0.
for input_data in training_input:
    output_data = network.compute_regression(input_data)
    print(str(input_data) + " -> " + str(output_data))