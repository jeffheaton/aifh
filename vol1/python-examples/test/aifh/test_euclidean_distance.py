#!/usr/bin/env python
# Programmer: Chris Bunch (shatterednirvana@gmail.com)


# General-purpose Python library imports
import os
import sys
import unittest


# Third party libraries
from flexmock import flexmock


# Import for the library that we're testing here
distance_dir = os.path.dirname(__file__) + os.sep + ".." + os.sep + ".." + \
  os.sep + "lib" + os.sep + "distance"
sys.path.append(distance_dir)
from euclidean_distance import EuclideanDistance


class TestEuclideanDistance(unittest.TestCase):


  def test_distance_calc(self):
    calc = EuclideanDistance()
    pos1 = [0.5, 1.0, 2.5]
    pos2 = [0.1, 2.0, -2.5]
    self.assertAlmostEqual(5.1146, calc.calculate(pos1, pos2), delta=0.001)
