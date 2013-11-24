#!/usr/bin/env python
# Programmer: Chris Bunch (shatterednirvana@gmail.com)


import os
import sys
import unittest


# imports for distance tests
from aifh.test_chebyshev_distance import TestChebyshevDistance
from aifh.test_euclidean_distance import TestEuclideanDistance
from aifh.test_manhattan_distance import TestManhattanDistance
from aifh.test_normalize import TestNormalize
from aifh.test_equilateral import TestEquilateral


test_cases = [TestChebyshevDistance, TestEuclideanDistance,
  TestManhattanDistance, TestNormalize, TestEquilateral]
test_suite = unittest.TestSuite()
for test_class in test_cases:
  tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
  test_suite.addTests(tests)

all_tests = unittest.TestSuite([test_suite])
unittest.TextTestRunner(verbosity=2).run(all_tests)
