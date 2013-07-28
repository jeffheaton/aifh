#!/usr/bin/env python
# Programmer: Chris Bunch (shatterednirvana@gmail.com)

import sys
import unittest


# imports for distance tests
from test_chebyshev_distance import TestChebyshevDistance


test_cases = [TestChebyshevDistance]
test_suite = unittest.TestSuite()
for test_class in test_cases:
  tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
  test_suite.addTests(tests)

all_tests = unittest.TestSuite([test_suite])
unittest.TextTestRunner(verbosity=2).run(all_tests)
