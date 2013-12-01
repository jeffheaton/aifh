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

import sys
import os
import numpy as np
from scipy.spatial import distance

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from train import TrainAnneal


class AnnealTSP(TrainAnneal):
    def perform_randomization(self, vec):
        # Find two cities to swap.
        while True:
            index1 = np.random.randint(0, CITY_COUNT)
            index2 = np.random.randint(0, CITY_COUNT)
            if index1 != index2:
                break;

        # Swap the two cities.
        vec[index1], vec[index2] = vec[index2], vec[index1]


def score_funct(x):
    result = 0
    for i in xrange(0, CITY_COUNT - 1):
        result += distance.euclidean(x[i], x[i + 1])
    return result


CITY_COUNT = 50
MAP_SIZE = 10

# Place the cities in a circle.
cities = []
ratio = (2.0 * np.pi) / CITY_COUNT

for i in xrange(0, CITY_COUNT):
    x = int(np.cos(ratio * i) * (MAP_SIZE / 2.0) + (MAP_SIZE / 2.0))
    y = int(np.sin(ratio * i) * (MAP_SIZE / 2.0) + (MAP_SIZE / 2.0))

    cities.append([x, y])


# Pick a random city order.  Here we are sampling without replacement.  This means choose 50 random integers but
# do not repeat.

current_path = []

while len(current_path) < CITY_COUNT:
    city_index = np.random.randint(0, CITY_COUNT)
    if city_index not in current_path:
        current_path.append(city_index)


# Run the annealing.

train = AnnealTSP()
train.display_iteration = True
train.max_iterations = 500
train.train(current_path, score_funct)
print("Final distance: " + str(score_funct(train.position)))
print("Final path: " + str(train.position))

