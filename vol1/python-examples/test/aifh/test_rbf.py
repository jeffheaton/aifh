__author__ = 'jheaton'

import os
import sys
import unittest
import numpy as np

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