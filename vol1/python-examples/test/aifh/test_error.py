__author__ = 'jheaton'

import os
import sys
import unittest
import numpy as np

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from equilateral import Equilateral
from scipy.spatial import distance
from error import ErrorCalculation



class TestError(unittest.TestCase):

    IDEAL = [
            [1.0, 2.0, 3.0, 4.0],
            [5.0, 6.0, 7.0, 8.0],
            [9.0, 10.0, 11.0, 12.0],
            [13.0, 14.0, 15.0, 16.0],
            [17.0, 18.0, 19.0, 20.0]
    ]

    ACTUAL = [
            [1.1, -2.0, -3.0, 4.1],
            [-5.1, -6.0, 7.1, 8.2],
            [9.1, 10.2, -11.5, 12.1],
            [13.0, -14.0, 15.0, 16.1],
            [17.0, 18.0, -19.0, 20.1]
    ]


    def test_rss(self):
        actual = np.array(TestError.ACTUAL)
        ideal = np.array(TestError.IDEAL)
        self.assertAlmostEqual(ErrorCalculation.rss(actual,ideal),3032.4099,3)

    def test_rms(self):
        actual = np.array(TestError.ACTUAL)
        ideal = np.array(TestError.IDEAL)
        self.assertAlmostEqual(ErrorCalculation.rms(actual,ideal),12.3134,3)

    def test_mse(self):
        actual = np.array(TestError.ACTUAL)
        ideal = np.array(TestError.IDEAL)
        self.assertAlmostEqual(ErrorCalculation.mse(actual,ideal),151.6205,3)