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

from rbf_network import RbfNetwork


class TestRbfNetwork(unittest.TestCase):
    def test_basics(self):
        network = RbfNetwork(2, 1, 1)

        # should be 7, (2*1) + (1+(1 bias))*1 + 3 RBF params
        # 2 + 2 + 3 = 7
        self.assertEquals(7, len(network.long_term_memory))


    def test_reset_compute(self):
        network = RbfNetwork(2, 1, 1)
        total = 0

        for i in range(0, len(network.long_term_memory)):
            total += network.long_term_memory[i]

        self.assertEquals(0, total)

        network.reset()

        total = 0
        for i in range(0, len(network.long_term_memory)):
            total += network.long_term_memory[i]

        self.assertTrue(total > 1)

    def test_compute_regression(self):
        network = RbfNetwork(2, 1, 1)

        network.copy_memory([
            2.0, # input 1 to RBF 1
            2.0, # input 2 to RBF 1
            5.0, # RBF width
            2.0, # RBF, center-0
            4.0, # RBF, center-1
            3.0, # RBF1 to Output 1
            4.0   # Bias to Output 1
        ])

        x = [1, 2]

        y = network.compute_regression(x)[0]

        # Inputs: (2*1) + (2*2) = 6
        # RBF: Gaussian(6) = 1
        # Outputs: (1*3) + (1*4) = 7
        self.assertAlmostEqual(7, y, 3)

    def test_compute_classification(self):
        network = RbfNetwork(2, 1, 2)

        network.copy_memory([
            2.0, # input 1 to RBF 1
            2.0, # input 2 to RBF 1
            5.0, # RBF width
            2.0, # RBF, center-0
            4.0, # RBF, center-1
            3.0, # RBF1 to Output 1
            4.0, # Bias to Output 1
            5.0, # RBF1 to Output 2
            6.0])  # Bias to Output 2

        x = [1, 2]

        y = network.compute_regression(x)

        # Inputs: (2*1) + (2*2) = 6
        # RBF: Gaussian(6) = 1
        # Outputs: (1*3) + (1*4) = 7
        self.assertAlmostEqual(7, y[0], 3)

        # Inputs: (2*1) + (2*2) = 6
        # RBF: Gaussian(6) = 1
        # Outputs: (1*5) + (1*6) = 11
        self.assertAlmostEqual(11, y[1], 3)

        cls = network.compute_classification(x)

        # class 1 is higher than class 0
        self.assertEqual(1, cls)