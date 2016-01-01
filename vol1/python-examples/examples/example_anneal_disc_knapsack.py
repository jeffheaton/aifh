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

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from train import TrainAnneal

# Number of items to choose from.
NUM_ITEMS_TO_CHOOSE = 25

# The max weight of the knapsack.
KNAPSACK_MAX_WEIGHT = 50

# The max weight for an item.
ITEM_MAX_WEIGHT = 20

# The max value for an item.
ITEM_MAX_VALUE = 1000

# Calculated later to be the maximum profit, if we were carrying everything.  Distance from this number becomes
# the score.
max_profit = 0

# The profit for each item.
profit = [0] * (NUM_ITEMS_TO_CHOOSE + 1)

# The weight for each item.
weight = [0] * (NUM_ITEMS_TO_CHOOSE + 1)


class AnnealKnapsack(TrainAnneal):
    """ Subclass the TrainAnneal to provide a randomization function to handle the Knapsack Problem.
        This example program shows how to use discrete simulated annealing to find solutions to the Knapsack problem.
        http://en.wikipedia.org/wiki/Knapsack_problem
    """

    def perform_randomization(self, vec):
        """ Randomize the current knapsack to some degree.  We do this by adding a random item.
        If this puts us over, then we re-balance.
        @param vec: The vector contents of the knapsack.
        @return: nothing.
        """
        # Check for strange case where we have everything!
        # This means that the max allowed knapsack weight is greater than the total of grabbing everything.
        # This is kind of pointless, but don't go into an endless loop!
        holding_everything_already = True
        for aCurrentTaken in vec:
            if not aCurrentTaken:
                holding_everything_already = False
                break
        # Normal case.  So try to add something.
        if not holding_everything_already:
            # try to add something
            # Prime the loop.
            pt = np.random.randint(0, len(vec))
            while vec[pt]:
                pt = np.random.randint(0, len(vec))

            # Add the item we found.
            vec[pt] = True

            # We probably need to drop something now.
            balance(vec)


def score_function(x):
    """ Score the Knapsack.  Lower scores are better.  The score is the difference between the current knapsack
    value and the max_profit.  If we are over, simply return max_profit.  If we have max_profit, then return 0.
    We would never have max_profit unless we had enough knapsack to hold everything.
    @param x: The knapsack vector to score.
    @return: The score.
    """
    if calculate_total_weight(x) > KNAPSACK_MAX_WEIGHT:
        return max_profit
    # Loop over count up the profit.
    result = 0
    for idx in range(0, len(x)):
        if x[idx]:
            result += profit[idx]
    # The profit is not the score.  We want a score we can minimize.  So the score is the distance between our
    # current profit, and the max profit.
    return max_profit - result


def calculate_total_weight(items):
    """ Calculate the total weight of a knapsack.
    @param items: The knapsack to calculate.
    @return: The total weight.
    """
    # Loop over the list and sum up the weight.
    result = 0
    for i in range(0, len(items)):
        if items[i]:
            result += weight[i]
    return result


def balance(items):
    """
    Balance the knapsack.  Remove items until it is blow its maximum weight.
    @param items:
    @return:
    """
    while calculate_total_weight(items) > KNAPSACK_MAX_WEIGHT:
        remove = np.random.randint(0, len(items))
        items[remove] = False

# Generate a random set of items.
# Calculate max_profit.
for n in range(0, NUM_ITEMS_TO_CHOOSE):
    profit[n] = int(np.random.uniform(1, ITEM_MAX_VALUE))
    weight[n] = int(np.random.uniform(1, ITEM_MAX_WEIGHT))
    max_profit += profit[n]

# Run the annealing.
train = AnnealKnapsack()
train.display_iteration = True
train.max_iterations = 500
train.train([0] * NUM_ITEMS_TO_CHOOSE, score_function)
print("Final profit: " + str(max_profit - score_function(train.position)))

# Display the results.
print("Took the following items: ")
for i in range(0, NUM_ITEMS_TO_CHOOSE):
    if train.position[i]:
        print("Item #" + str(i) + ", weight=" + str(weight[i]) + ", profit=" + str(profit[i]))