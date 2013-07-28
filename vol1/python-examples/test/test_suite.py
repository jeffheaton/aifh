#!/usr/bin/env python
# Programmer: Chris Bunch (shatterednirvana@gmail.com)

import sys
import unittest


# imports for distance tests
from test_chebyshev_distance import TestChebyshevDistance
from test_euclidean_distance import TestEuclideanDistance
from test_manhattan_distance import TestManhattanDistance


test_cases = [TestChebyshevDistance, TestEuclideanDistance,
  TestManhattanDistance]
test_suite = unittest.TestSuite()
for test_class in test_cases:
  tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
  test_suite.addTests(tests)

all_tests = unittest.TestSuite([test_suite])
unittest.TextTestRunner(verbosity=2).run(all_tests)
