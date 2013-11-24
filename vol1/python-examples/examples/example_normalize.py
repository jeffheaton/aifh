__author__ = 'jheaton'

import os
import sys

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")                           
sys.path.append(aifh_dir)

from normalize import Normalize


# find the Iris data set
irisFile = os.path.dirname(os.path.realpath(__file__))
irisFile = os.path.abspath(irisFile + "../../datasets/iris.csv")

# Read the Iris data set.
print('Reading CSV file: ' + irisFile)
norm = Normalize()
result = norm.load_csv(irisFile)

# Setup the first four fields to "range normalize" between -1 and 1.
for i in range(0, 4):
    norm.make_col_numeric(result, i)
    norm.norm_col_range(result, i, -1, 1)

# Discover all of the classes for column #4, the iris species.
classes = norm.build_class_map(result, 4)

# Normalize iris species as one-of-n
norm.norm_col_equilateral(result, 4, classes, -1, 1)

# Display the resulting data
norm.display_data(result)

