import unittest
import os
import sys

__author__ = 'jheaton'


# Import for the library that we're testing here
aifh_dir = os.path.dirname(__file__) + os.sep + ".." + \
           os.sep + ".." + os.sep + "lib" + os.sep + "aifh"
sys.path.append(aifh_dir)

from normalize import Normalize
from aifh_error import AIFHError


class TestNormalize(unittest.TestCase):
    def test_normalize_one_of_n(self):
        # find the Iris data set
        irisFile = os.path.dirname(os.path.realpath(__file__))
        irisFile = os.path.abspath(irisFile + "../../../datasets/iris.csv")

        norm = Normalize()

        result = norm.load_csv(irisFile)

        self.assertEqual(len(norm.column_map), 5)
        self.assertEqual(len(norm.header), 5)
        self.assertEqual(norm.header[0], "sepal_length")
        self.assertEqual(norm.header[1], "sepal_width")
        self.assertEqual(norm.header[2], "petal_length")
        self.assertEqual(norm.header[3], "petal_width")
        self.assertEqual(norm.header[4], "class")
        self.assertTrue("sepal_length" in norm.column_map)
        self.assertTrue("sepal_width" in norm.column_map)
        self.assertTrue("petal_length" in norm.column_map)
        self.assertTrue("petal_width" in norm.column_map)
        self.assertTrue("class" in norm.column_map)
        self.assertEqual(norm.resolve_column("sepal_length"), 0)
        self.assertEqual(norm.resolve_column("sepal_width"), 1)
        self.assertEqual(norm.resolve_column("petal_length"), 2)
        self.assertEqual(norm.resolve_column("petal_width"), 3)
        self.assertEqual(norm.resolve_column("class"), 4)
        self.assertRaises(AIFHError, norm.resolve_column, 6)
        self.assertRaises(AIFHError, norm.resolve_column, "unknown")

        for i in range(0, 4):
            norm.make_col_numeric(result, i)
            norm.norm_col_range(result, i, -1, 1)

        self.assertAlmostEqual(result[0][0], -0.555, 2)
        self.assertAlmostEqual(result[0][1], 0.249, 2)
        self.assertAlmostEqual(result[0][2], -0.864, 2)
        self.assertAlmostEqual(result[0][3], -0.916, 2)

        classes = norm.build_class_map(result, 4)
        norm.norm_col_one_of_n(result, 4, classes, -1, 1)
        self.assertEqual(len(classes), 3)

    def test_normalize_equilateral(self):
        # find the Iris data set
        irisFile = os.path.dirname(os.path.realpath(__file__))
        irisFile = os.path.abspath(irisFile + "../../../datasets/iris.csv")

        norm = Normalize()
        result = norm.load_csv(irisFile)
        classes = norm.build_class_map(result, 4)
        norm.norm_col_equilateral(result, 4, classes, 0, 1)
        self.assertEqual(len(result[0]), 6)
        self.assertAlmostEqual(result[0][4], 0.06698, 3)


