__author__ = 'jheaton'

import os
import csv

import numpy as np

f = open("file.txt")
# skip over header info
X = np.loadtxt(f)
max_per_col = X.max(axis=0)
max_per_row = X.max(axis=1)


def loadCSV(filename):
    result = []
    with open(filename, 'rt') as f:
        reader = csv.reader(f)
        for row in reader:
            result.append(row)
    return result

def displayData(dataset):
    for row in dataset:
        print(row)

def normMax(dataset,col):
    result = sys.float_info.min


# find the Iris data set
irisFile = os.path.dirname(os.path.realpath(__file__))
irisFile = os.path.abspath(irisFile + "../../datasets/iris.csv")

print('Reading CSV file: ' + irisFile)

result = loadCSV(irisFile)
displayData(result)

