#!/usr/bin/env python
# Programmer: Chris Bunch (shatterednirvana@gmail.com)


import os
import sys
import unittest


# imports for distance tests
from distance.test_chebyshev_distance import TestChebyshevDistance
from distance.test_euclidean_distance import TestEuclideanDistance
from distance.test_manhattan_distance import TestManhattanDistance


test_cases = [TestChebyshevDistance, TestEuclideanDistance,
  TestManhattanDistance]
test_suite = unittest.TestSuite()
for test_class in test_cases:
  tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
  test_suite.addTests(tests)

all_tests = unittest.TestSuite([test_suite])
unittest.TextTestRunner(verbosity=2).run(all_tests)
