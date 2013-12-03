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
This example shows how three different distance metrics calculate the same three points.

Euclidean Distance
pos1->pos2: 5.19615242271
pos2->pos3: 5.19615242271
pos3->pos1: 10.3923048454

Manhattan (city block) Distance

pos1->pos2: 9.0
pos2->pos3: 9.0
pos3->pos1: 18.0

Chebyshev Distance

pos1->pos2: 3.0
pos2->pos3: 3.0
pos3->pos1: 6.0
"""
__author__ = 'jheaton'

import os
import sys
from scipy.spatial import distance

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

# Create three different positions.
pos1 = [1.0, 2.0, 3.0]
pos2 = [4.0, 5.0, 6.0]
pos3 = [7.0, 8.0, 9.0]


# Calculate the distance between the specified points in 3 metrics.
print("Euclidean Distance")
print("pos1->pos2: " + str(distance.euclidean(pos1, pos2)))
print("pos2->pos3: " + str(distance.euclidean(pos2, pos3)))
print("pos3->pos1: " + str(distance.euclidean(pos3, pos1)))
print("\nManhattan (city block) Distance\n")
print("pos1->pos2: " + str(distance.cityblock(pos1, pos2)))
print("pos2->pos3: " + str(distance.cityblock(pos2, pos3)))
print("pos3->pos1: " + str(distance.cityblock(pos3, pos1)))
print("\nChebyshev Distance\n")
print("pos1->pos2: " + str(distance.chebyshev(pos1, pos2)))
print("pos2->pos3: " + str(distance.chebyshev(pos2, pos3)))
print("pos3->pos1: " + str(distance.chebyshev(pos3, pos1)))
