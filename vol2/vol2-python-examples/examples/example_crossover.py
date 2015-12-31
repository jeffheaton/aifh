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
    This example uses crossover to combine two parent genomes to produce two children.
    Both repeating and non-repeating splice are used.

Crossover Splice
Parent 1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Parent 2: [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
Offspring 1: [1, 2, 3, 4, 5, 6, 4, 3, 2, 1]
Offspring 2: [10, 9, 8, 7, 6, 5, 7, 8, 9, 10]

Crossover Splice No Repeat
Parent 1: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Parent 2: [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
Offspring 1: [10, 3, 2, 4, 5, 6, 7, 8, 9, 1]
Offspring 2: [1, 8, 9, 7, 6, 5, 4, 3, 2, 10]

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



p1 = [ 1,2,3,4,5,6,7,8,9,10 ]
p2 = [ 10,9,8,7,6,5,4,3,2,1 ]
off = [[],[]]

pop = Population()

print("Crossover Splice")
crossover_splice(pop, p1,p2,off)
print("Parent 1: " + str(p1))
print("Parent 2: " + str(p2))
print("Offspring 1: " + str(off[0]))
print("Offspring 2: " + str(off[1]))

print()
print("Crossover Splice No Repeat")
crossover_splice_no_repeat(pop, p1,p2,off)
print("Parent 1: " + str(p1))
print("Parent 2: " + str(p2))
print("Offspring 1: " + str(off[0]))
print("Offspring 2: " + str(off[1]))