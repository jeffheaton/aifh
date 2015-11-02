#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 3: Deep Learning and Neural Networks
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com
    Code repository:
    https://github.com/jeffheaton/aifh
    Copyright 2015 by Jeff Heaton
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
import os
import sys

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from dbn import *
import numpy as np

LEARNING_RATE_UNSUPERVISED = 0.1
LEARNING_RATE_SUPERVISED = 0.1
K = 1

# training data
TRAINING_INPUT = np.array([
            [1, 1, 1, 1, 0, 0, 0, 0],
            [1, 1, 0, 1, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 1, 1, 1, 1],
            [0, 0, 0, 0, 1, 1, 0, 1],
            [0, 0, 0, 0, 1, 1, 1, 0]])

TRAINING_IDEAL = np.array([
            [1, 0],
            [1, 0],
            [1, 0],
            [0, 1],
            [0, 1],
            [0, 1]])

TEST_INPUT = np.array([
            [0, 1, 1, 1, 0, 0, 0, 0],
            [1, 0, 1, 1, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 1, 0, 1, 1]])


# Create an deep belief network.
hidden = [2, 3]
dbn = DeepBeliefNetwork(len(TRAINING_INPUT[0]), hidden, len(TRAINING_IDEAL[0]))
dbn.reset()

# Layer by layer unsupervised training.
for level in range(len(hidden)):
    train_unsupervised = UnsupervisedTrainDBN(
                    dbn,level,TRAINING_INPUT,LEARNING_RATE_UNSUPERVISED,K)
    for i in range(2000):
        print("Training unsupervised: Layer: {}, Iteration: {}".format(level, i))
        train_unsupervised.iteration()

# Supervised training.
print("Beginning supervised training...")
train_supervised = SupervisedTrainDBN(dbn,TRAINING_INPUT,TRAINING_IDEAL,LEARNING_RATE_SUPERVISED)
iteration = 0
done = False
while not done:
    iteration+=1
    train_supervised.iteration();
    print("Iteration: {}, Supervised training: error = {}".format(iteration,train_supervised.error()))
    if train_supervised.error()<0.001:
        done = True


# Use test data.
for row in TEST_INPUT:
    output = dbn.compute_regression(row)
    print("{} -> {}".format(row,output))

