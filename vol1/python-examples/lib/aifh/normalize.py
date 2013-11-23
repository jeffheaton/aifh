__author__ = 'jheaton'

import csv
import sys
from aifh_error import AIFHError

class Normalize(object):
    """Normalize utility"""

    def __init__(self):
        self.header = []
        self.column_map = {}

    def load_csv(self, filename):
        result = []
        first = True

        with open(filename, 'rt') as f:
            reader = csv.reader(f)
            for row in reader:
                if len(row) > 0:
                    if first:
                        first = False
                        self.header = row
                    else:
                        result.append(row)

        for idx in range(0, len(self.header)):
            self.column_map[self.header[idx]] = idx
        return result

    @staticmethod
    def display_data(data_set):
        for row in data_set:
            print(row)

    def max(self, data_set, col):
        col = self.resolve_column(col)
        result = sys.float_info.min
        for row in data_set:
            result = max(result, row[col])
        return result

    def min(self, data_set, col):
        col = self.resolve_column(col)
        result = sys.float_info.max
        for row in data_set:
            result = min(result, row[col])
        return result

    def make_numeric(self, data_set, col):
        col = self.resolve_column(col)
        for row in data_set:
            row[col] = float(row[col])

    def norm_range(self, data_set, col, normalized_low, normalized_high):
        col = self.resolve_column(col)
        data_low = self.min(data_set, col)
        data_high = self.max(data_set, col)

        for row in data_set:
            row[col] = ((row[col] - data_low) / (data_high - data_low)) \
                * (normalized_high - normalized_low) + normalized_low

    def build_class_map(self, data_set, col):
        col = self.resolve_column(col)
        result = {}
        index = 0
        for row in data_set:
            key = row[col]
            if key not in result:
                result[key] = index
                index += 1
        return result

    def norm_one_of_n(self, data_set, col, classes):
        col = self.resolve_column(col)

        for row in data_set:
            key = row[col]
            value = classes[key]
            row.pop(col)
            for i in range(0, len(classes)):
                if i == value:
                    row.insert(col + i, 1)
                else:
                    row.insert(col + 1, 0)

    def resolve_column(self, col):
        if type(col) is int:
            if col < 0 or col >= len(self.column_map):
                raise AIFHError("Column index out of range: " + str(col) )
            return col
        else:
            if col not in self.column_map:
                raise AIFHError("Undefined column: " + col  )
            else:
                return self.column_map[col]