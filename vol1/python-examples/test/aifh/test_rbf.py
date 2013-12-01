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

import os
import sys
import unittest

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from rbf import RbfGaussian
from rbf import RbfMexicanHat
from rbf import RbfMultiquadric
from rbf import RbfInverseMultiquadric


class TestRBF(unittest.TestCase):
    def test_gaussian(self):
        params = [5, 0, 0, 0]
        funct = RbfGaussian(3, params, 0);
        x = [-1, 0, 1]
        y = funct.evaluate(x);
        self.assertEquals(0.9607894391523232, y, 7)

    def test_mexican(self):
        params = [5, 0, 0, 0]
        funct = RbfMexicanHat(3, params, 0);
        x = [-1, 0, 1]
        y = funct.evaluate(x);
        self.assertEquals(-0.36787944117144233, y, 7)

    def test_multiquadric(self):
        params = [5, 0, 0, 0]
        funct = RbfMultiquadric(3, params, 0);
        x = [-1, 0, 1]
        y = funct.evaluate(x);
        self.assertEquals(8.774964387392123, y, 7)

    def test_inv_multiquadric(self):
        params = [5, 0, 0, 0]
        funct = RbfInverseMultiquadric(3, params, 0);
        x = [-1, 0, 1]
        y = funct.evaluate(x);
        self.assertEquals(0.11396057645963795, y, 7)