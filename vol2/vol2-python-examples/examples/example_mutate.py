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
    This example uses mutation to create an offspring based on a parent.  Both shuffle and perturb mutate
    are used.

Mutate Shuffle
Parent: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Offspring: [1, 9, 3, 4, 5, 6, 7, 8, 2, 10]

Mutate Perturb
Parent: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Offspring: [1.0181826792802973, 2.086185519941178, 3.1626939783521824, 3.836760183634765, 5.097865522694047,
5.832560686901486, 6.754812862640574, 7.669594913908851, 9.889151663395923, 10.252360819747098]

"""

import sys
import os

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from genetic import *

p1 = [ 1,2,3,4,5,6,7,8,9,10 ]
off = [0] * 10

pop = Population()

print("Mutate Shuffle")
mutate_shuffle(pop, p1,off)
print("Parent: " + str(p1))
print("Offspring: " + str(off))

print()
print("Mutate Perturb")
mutate_perturb(pop, p1,off)
print("Parent: " + str(p1))
print("Offspring: " + str(off))