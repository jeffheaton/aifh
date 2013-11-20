__author__ = 'jheaton'

import csv
import os

# find the Iris data set
irisFile = os.path.dirname(os.path.realpath(__file__))
irisFile = os.path.abspath(irisFile + "../../datasets/iris.csv")

print('Reading CSV file: ' + irisFile)

with open(irisFile, 'rt') as f:
    reader = csv.reader(f)
    for row in reader:
        print(row)