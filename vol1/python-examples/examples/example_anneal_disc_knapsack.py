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
NUM_ITEMS_TO_CHOOSE = 25;

# The max weight of the knapsack.
KNAPSACK_MAX_WEIGHT = 50;

# The max weight for an item.
ITEM_MAX_WEIGHT = 20;

# The max value for an item.
ITEM_MAX_VALUE = 1000;

max_profit = 0

# The profit for each item.
profit = [0] * (NUM_ITEMS_TO_CHOOSE + 1);

# The weight for each item.
weight = [0] * (NUM_ITEMS_TO_CHOOSE + 1);


class AnnealKnapsack(TrainAnneal):
    def perform_randomization(self, vec):
        # Check for strange case where we have everything!
        # This means that the max allowed knapsack weight is greater than the total of grabbing everything.
        # This is kind of pointless, but don't go into an endless loop!
        holdingEverythingAlready = True;
        for aCurrentTaken in vec:
            if not aCurrentTaken:
                holdingEverythingAlready = False;
                break

        if not holdingEverythingAlready:
            # try to add something
            pt = np.random.randint(0, len(vec)); # Prime the loop.
            while vec[pt] == True:
                pt = np.random.randint(0, len(vec)); # Prime the loop.

            # Add the item we found.
            vec[pt] = True;

            # We probably need to drop something now.
            balance(vec);


def score_funct(x):
    if calculate_total_weight(x) > KNAPSACK_MAX_WEIGHT:
        return max_profit;

    result = 0;
    for i in xrange(0, len(x)):
        if x[i]:
            result += profit[i];
    return max_profit - result


def calculate_total_weight(items):
    result = 0;
    for i in xrange(0, len(items)):
        if items[i]:
            result += weight[i]
    return result


def balance(items):
    while calculate_total_weight(items) > KNAPSACK_MAX_WEIGHT:
        remove = np.random.randint(0, len(items))
        items[remove] = False

# Generate a random set of items.
max_profit = 0
for n in xrange(0, NUM_ITEMS_TO_CHOOSE):
    profit[n] = int(np.random.uniform(1, ITEM_MAX_VALUE));
    weight[n] = int(np.random.uniform(1, ITEM_MAX_WEIGHT));
    max_profit += profit[n]

# Run the annealing.

train = AnnealKnapsack()
train.display_iteration = True
train.max_iterations = 500
train.train([0] * NUM_ITEMS_TO_CHOOSE, score_funct)
print("Final profit: " + str(max_profit - score_funct(train.position)))
print("Took the following items: " )
for i in xrange(0, NUM_ITEMS_TO_CHOOSE):
    if train.position[i]:
        print "Item #" + str(i) + ", weight=" + str(weight[i]) + ", profit=" + str(profit[i])

