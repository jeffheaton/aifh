#!/usr/bin/env python
# Programmer: Chris Bunch (shatterednirvana@gmail.com)

# General-purpose Python library imports
import os
import sys
import unittest

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from euclidean_distance import EuclideanDistance


class TestEuclideanDistance(unittest.TestCase):


  def test_distance_calc(self):
    calc = EuclideanDistance()
    pos1 = [0.5, 1.0, 2.5]
    pos2 = [0.1, 2.0, -2.5]
    self.assertAlmostEqual(5.1146, calc.calculate(pos1, pos2), delta=0.001)
