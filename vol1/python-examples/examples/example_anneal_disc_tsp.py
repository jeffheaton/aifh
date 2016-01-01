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
    This example uses discrete simulated annealing with the Traveling Salesman Problem (TSP).  The cities are placed
    in a circle, so the ideal path is known.  Because the cities are in a circle they should be visited in order
    around the edge for the absolute optimal path.
    http://en.wikipedia.org/wiki/Traveling_salesman_problem

    Running the program produces the following output.  Simulated annealing always performs the maximum number of
    iterations, unless stopped early.  Here we do not specify a stopping score, so the full 500 iterations are used.
    Each line of output shows the iteration number, the score, the k (also the iteration number), kmax (the max
    number of iterations), t (the temperature), and prob (the probability of accepting a worse solution than the
    current).  You can see that an optimal solution was not found, but we are close!  There are large ranges of
    numbers in order.  We stay close to the edge of the circle.

/Users/jheaton/anaconda/bin/python /Users/jheaton/projects/aifh/vol1/python-examples/examples/example_anneal_disc_tsp.py
Iteration #1, Score: 649.0,k=1,kMax=500,t=388.021572487,prob=0.979593675247,699.0
Iteration #2, Score: 649.0,k=2,kMax=500,t=376.401851787,prob=0.938228607478,809.0
Iteration #3, Score: 649.0,k=3,kMax=500,t=365.130096043,prob=0.967669187719,826.0
Iteration #4, Score: 637.0,k=4,kMax=500,t=354.195885072,prob=0.99156587898,746.0
Iteration #5, Score: 637.0,k=5,kMax=500,t=343.589110735,prob=0.937976889178,752.0
Iteration #6, Score: 637.0,k=6,kMax=500,t=333.299967592,prob=0.96463681706,770.0
Iteration #7, Score: 637.0,k=7,kMax=500,t=323.318943837,prob=1.0,897.0
Iteration #8, Score: 637.0,k=8,kMax=500,t=313.636812505,prob=1.0,952.0
Iteration #9, Score: 637.0,k=9,kMax=500,t=304.244622945,prob=0.993447901548,738.0
Iteration #10, Score: 637.0,k=10,kMax=500,t=295.133692539,prob=0.934479210491,849.0
...
Iteration #497, Score: 70.0,k=497,kMax=500,t=0.000109549994463,prob=0.0,70.0
Iteration #498, Score: 70.0,k=498,kMax=500,t=0.000106269402794,prob=0.0,70.0
Iteration #499, Score: 70.0,k=499,kMax=500,t=0.000103087051948,prob=0.0,70.0
Iteration #500, Score: 70.0,k=500,kMax=500,t=0.0001,prob=0.0,70.0
Finished after 501 iterations, final score is 70.0
Final distance: 70.0
Final path: [28, 29, 30, 31, 32, 33, 34, 35, 36, 38, 40, 42, 43, 44, 45, 47, 48, 49, 46, 41, 39, 37, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]

Process finished with exit code 0


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
    """
    Use simulated annealing with the Traveling Salesman Problem (TSP).  The cities are placed in a circle, so the
    ideal path is known.  Because the cities are in a circle they should be visited in order for the absolute
    optimal path.

    http://en.wikipedia.org/wiki/Traveling_salesman_problem
    """
    def perform_randomization(self, vec):
        """
        Randomize by swapping two cities in the path.
        @param vec: The path to randomize.
        """
        # Find two cities to swap.
        while True:
            index1 = np.random.randint(0, CITY_COUNT)
            index2 = np.random.randint(0, CITY_COUNT)
            if index1 != index2:
                break

        # Swap the two cities.
        vec[index1], vec[index2] = vec[index2], vec[index1]


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

# Place the cities in a circle.
cities = []
ratio = (2.0 * np.pi) / CITY_COUNT

for i in range(0, CITY_COUNT):
    x = int(np.cos(ratio * i) * (MAP_SIZE / 2.0) + (MAP_SIZE / 2.0))
    y = int(np.sin(ratio * i) * (MAP_SIZE / 2.0) + (MAP_SIZE / 2.0))

    cities.append([x, y])


# Pick a random city order.  Here we are sampling without replacement.  This means choose 50 random integers but
# do not repeat.  We only want to visit a city once.
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

# Display results.
print("Final distance: " + str(score_funct(train.position)))
print("Final path: " + str(train.position))

