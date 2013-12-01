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

# General-purpose Python library imports
import os
import sys
import unittest


# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from equilateral import Equilateral
from scipy.spatial import distance


class TestEquilateral(unittest.TestCase):
    def test_equilateral(self):
        eq = Equilateral(3, -1, 1)
        d = eq.encode(1);
        self.assertAlmostEqual(0.8660254037844386, d[0], 7)
        self.assertAlmostEqual(-0.5, d[1], 7)

    def test_decode(self):
        eq = Equilateral(3, -1, 1)
        d0 = [0.866, 0.5]
        d1 = [-0.866, 0.5]
        d2 = [0, -1]
        self.assertEqual(2, eq.decode(d0))
        self.assertEqual(2, eq.decode(d1))
        self.assertEqual(0, eq.decode(d2))

    def test_all_equal(self):
        eq = Equilateral(10, -1, 1)
        compare_dist = -1

        for x in range(0, 10):
            base_class = eq.encode(x)
            for y in range(0, 10):
                if x != y:
                    otherClass = eq.encode(y)
                    dist = distance.euclidean(base_class, otherClass)
                    if compare_dist < 0:
                        compare_dist = dist
                    else:
                        self.assertAlmostEqual(dist, compare_dist, 7)

