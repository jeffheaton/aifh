#!/usr/bin/env python
# Programmer: Chris Bunch (shatterednirvana@gmail.com)


# General-purpose Python library imports
import os
import sys
import unittest


# Third party libraries
from flexmock import flexmock


# Import for the library that we're testing here
lib = os.path.dirname(__file__) + os.sep + ".." + os.sep
sys.path.append(lib)
from distance import Distance


class TestDistance(unittest.TestCase):


  def test_nothing_yet(self):
    pass
