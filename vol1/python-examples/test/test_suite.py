#!/usr/bin/env python
# Programmer: Chris Bunch, Brian Drawert

import sys
import unittest


# imports for appscale library tests
from test_distance import TestDistance


test_cases = [TestDistance]
test_suite = unittest.TestSuite()
for test_class in test_cases:
  tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
  test_suite.addTests(tests)

all_tests = unittest.TestSuite([test_suite])
unittest.TextTestRunner(verbosity=2).run(all_tests)
