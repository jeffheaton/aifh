__author__ = 'jheaton'

import os
import sys
import unittest
import numpy as np

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
        self.assertEquals(7, len(network.longTermMemory))


    def test_reset_compute(self):
        network = RbfNetwork(2, 1, 1)
        total = 0

        for i in xrange(0, len(network.longTermMemory)):
            total += network.longTermMemory[i]

        self.assertEquals(0, total)

        network.reset()

        total = 0
        for i in xrange(0, len(network.longTermMemory)):
            total += network.longTermMemory[i]

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

        y = network.computeRegression(x)[0]

        # Inputs: (2*1) + (2*2) = 6
        # RBF: Gaussian(6) = 1
        # Outputs: (1*3) + (1*4) = 7
        self.assertAlmostEqual(7, y, 3)

    def test_compute_classification(self):
        network = RbfNetwork(2, 1, 2)

        network.copy_memory([
            2.0,   # input 1 to RBF 1
            2.0,   # input 2 to RBF 1
            5.0,   # RBF width
            2.0,   # RBF, center-0
            4.0,   # RBF, center-1
            3.0,   # RBF1 to Output 1
            4.0,   # Bias to Output 1
            5.0,   # RBF1 to Output 2
            6.0])  # Bias to Output 2

        x = [1, 2]

        y = network.computeRegression(x)

        # Inputs: (2*1) + (2*2) = 6
        # RBF: Gaussian(6) = 1
        # Outputs: (1*3) + (1*4) = 7
        self.assertAlmostEqual(7, y[0], 3)

        # Inputs: (2*1) + (2*2) = 6
        # RBF: Gaussian(6) = 1
        # Outputs: (1*5) + (1*6) = 11
        self.assertAlmostEqual(11, y[1], 3)

        cls = network.compure_classification(x)

        # class 1 is higher than class 0
        self.assertEqual(1, cls)