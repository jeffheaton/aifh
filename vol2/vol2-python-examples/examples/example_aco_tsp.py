#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 2: Nature-Inspired Algorithms
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com

    Code repository:
    https://github.com/jeffheaton/aifh

    Copyright 2014 by Jeff Heaton

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
    This example uses a discrete ant colony algorithm with the Traveling Salesman Problem (TSP).  The cities are placed
    in a circle, so the ideal path is known.  Because the cities are in a circle they should be visited in order
    around the edge for the absolute optimal path. In this case the cities should be arranged in numeric order to
    indicate that we are following the edge of the circle.

    http://en.wikipedia.org/wiki/Traveling_salesman_problem

    Running the program produces the following output.  Generations continue until the score remains stagnant for 10
    generations, or the maximum number of allowed generations.

Generaton #1, Score=575.0, stagnant=0
Generaton #2, Score=547.0, stagnant=0
Generaton #3, Score=499.0, stagnant=0
Generaton #4, Score=467.0, stagnant=0
Generaton #5, Score=425.0, stagnant=0
Generaton #6, Score=411.0, stagnant=0
Generaton #7, Score=399.0, stagnant=0
Generaton #8, Score=369.0, stagnant=0
Generaton #9, Score=355.0, stagnant=0
...
Generaton #69, Score=71.0, stagnant=1
Generaton #70, Score=71.0, stagnant=2
Generaton #71, Score=71.0, stagnant=3
Generaton #72, Score=71.0, stagnant=4
Generaton #73, Score=71.0, stagnant=5
Generaton #74, Score=71.0, stagnant=6
Generaton #75, Score=71.0, stagnant=7
Generaton #76, Score=71.0, stagnant=8
Generaton #77, Score=71.0, stagnant=9
Final distance: 71.0
Final path: [27, 28, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 41, 43, 44, 45, 47, 48, 49, 46, 42, 35,
29, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]


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

from aco import *

def score_funct(x):
    """
    The score function simply returns the distance covered by that path.
    @param x: The path to evaluate.
    @return: The Euclidean distance between each city on the path.
    """
    result = 0
    for i in range(0, CITY_COUNT - 1):
        result += distance.euclidean(x[i], x[i + 1])
    return result


CITY_COUNT = 50
MAP_SIZE = 10
POPULATION_SIZE = 1000

# Place the cities in a circle.
cities = []
ratio = (2.0 * np.pi) / CITY_COUNT

for i in range(0, CITY_COUNT):
    x = int(np.cos(ratio * i) * (MAP_SIZE / 2.0) + (MAP_SIZE / 2.0))
    y = int(np.sin(ratio * i) * (MAP_SIZE / 2.0) + (MAP_SIZE / 2.0))

    cities.append([x, y])

def calculate_cost(a,b):
    d = distance.euclidean(cities[a], cities[b])
    d = max(d,0.0001)
    return d


# Create a random population
train = DiscreteACO(50,CITY_COUNT,calculate_cost)
train.display_iteration = True
train.train()

# Display results.
print("Final distance: " + str(train.best_cost) )
print("Final path: " + str(train.best_path))

