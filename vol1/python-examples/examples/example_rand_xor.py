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
from train import TrainGreedRandom


training_input = np.array([
    [0.0, 0.0],
    [1.0, 0.0],
    [0.0, 1.0],
    [1.0, 1.0]])

training_ideal = np.array([
    [0.0],
    [1.0],
    [1.0],
    [0.0],
])

network = RbfNetwork(2, 5, 1)
network.reset()


def score_funct(x):
    network.copy_memory(x)
    actual_output = []
    for input_data in training_input:
        output_data = network.compute_regression(input_data)
        actual_output.append(output_data)
    return ErrorCalculation.mse(np.array(actual_output), training_ideal)


x0 = network.longTermMemory[:]
print(score_funct(x0))

train = TrainGreedRandom(-1, 1)
train.display_iteration = True
train.max_iterations = 500000
train.stop_score = 0.05
train.train(x0, score_funct)

network.copy_memory(train.position)

for input_data in training_input:
    output_data = network.compute_regression(input_data)
    print(str(input_data) + " -> " + str(output_data))