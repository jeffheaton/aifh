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
    This example uses tournament selection to select from a population of 1000 genomes that each have scores
    ranging from 0 to 999.  As you can see, when more tournament rounds are used, we typically select better
    genomes.  The average score reported is the average over 100k selections.

Rounds: 1, Avg Score: 665.08079
Rounds: 2, Avg Score: 747.69885
Rounds: 3, Avg Score: 798.4654
Rounds: 4, Avg Score: 831.841
Rounds: 5, Avg Score: 855.49337
Rounds: 6, Avg Score: 873.58435
Rounds: 7, Avg Score: 886.94374
Rounds: 8, Avg Score: 898.44297
Rounds: 9, Avg Score: 907.57187
Rounds: 10, Avg Score: 915.00512

"""

import sys
import os

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from genetic import *
from genetic import Genome
from genetic import Species

population = Population()
species = Species(population)

# Construct a simple population
for i in range(0,999):
    genome = Genome()
    genome.score = i
    species.members.append(genome)


# Perform the test for round counts between 1 and 10.
for round_count in range(1,11):
    selection = TournamentSelection()
    selection.rounds = round_count

    sum = 0
    count = 0;

    for i in range(0, 100000):
        genome = selection.select(species)
        sum = sum + genome.score
        count = count + 1

    sum = sum / count

    print("Rounds: " + str(round_count) + ", Avg Score: " + str(sum))






