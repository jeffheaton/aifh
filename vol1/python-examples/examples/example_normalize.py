__author__ = 'jheaton'

import os
import csv
import sys


def load_csv(filename):
    result = []
    first = True

    with open(filename, 'rt') as f:
        reader = csv.reader(f)
        for row in reader:
            if len(row) > 0:
                if first:
                    first = False
                    header = row
                else:
                    result.append(row)
    return result


def displayData(dataset):
    for row in dataset:
        print(row)


def normMax(dataset, col):
    result = sys.float_info.min
    for row in dataset:
        result = max(result, row[col])
    return result


def normMin(dataset, col):
    result = sys.float_info.max
    for row in dataset:
        result = min(result, row[col])
    return result


def normMakeNumeric(dataset, col):
    for row in dataset:
        row[col] = float(row[col])


def normRange(dataset, col, normalizedLow, normalizedHigh):
    dataLow = normMin(dataset, col)
    dataHigh = normMax(dataset, col)

    for row in dataset:
        row[col] = ((row[col] - dataLow) / (dataHigh - dataLow)) * (normalizedHigh - normalizedLow) + normalizedLow


def buildClassMap(dataset, col):
    result = {}
    index = 0
    for row in dataset:
        key = row[col]
        if key not in result:
            result[key] = index
            index += 1
    return result


def normOneOfN(dataset, col, classes):
    for row in dataset:
        key = row[col]
        value = classes[key]
        row.pop(col)
        for i in range(0, len(classes)):
            if i == value:
                row.insert(col + i, 1)
            else:
                row.insert(col + 1, 0)

# find the Iris data set
irisFile = os.path.dirname(os.path.realpath(__file__))
irisFile = os.path.abspath(irisFile + "../../datasets/iris.csv")

print('Reading CSV file: ' + irisFile)

result = loadCSV(irisFile)
for i in range(0, 4):
    normMakeNumeric(result, i)
    normRange(result, i, -1, 1)

classes = buildClassMap(result, 4)
normOneOfN(result, 4, classes)
displayData(result)

print(classes)

