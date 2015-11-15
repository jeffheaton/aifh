#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 3: Deep Learning and Neural Networks
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com
    Code repository:
    https://github.com/jeffheaton/aifh
    Copyright 2015 by Jeff Heaton
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
        http://www.apache.org/licenses/LICENSE-2.0
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
    For more information on Heaton Research copyrights, licenses
    and trademarks visit:
    http://www.heatonresearch.com/copyright
"""
__author__ = 'jheaton'

import csv
import sys
import numpy as np
from aifh_error import AIFHError
from equilateral import Equilateral


class Normalize(object):
    """ This class is used handle both nomalization and denormalization. The data is typically loaded in from a CSV
        file. Methods are provided to also normalize and denormalize individual numbers.
    """

    def __init__(self):
        """ Setup the normalize class.
        """
        self.header = []
        self.column_map = {}

    def load_csv(self, filename):
        """ Load a CSV file. The CSV file is assumed to have column headers as the first row. The headers will be read,
            and can be used to reference individual columns.  The columns can also be referenced by index.

        """
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
        """ Display a 2D data set to the console.

        """
        for row in data_set:
            print(row)

    def max(self, data_set, col):
        """ Obtain the maximum numeric value for the specified column.
            Note: this will not convert text values to numeric, see make_numeric for that.
        """
        col = self.resolve_column(col)
        result = sys.float_info.min
        for row in data_set:
            result = max(result, row[col])
        return result

    def min(self, data_set, col):
        """ Obtain the minimum numeric value for the specified column.
            Note: this will not convert text values to numeric, see make_numeric for that.
        """
        col = self.resolve_column(col)
        result = sys.float_info.max
        for row in data_set:
            result = min(result, row[col])
        return result

    def make_col_numeric(self, data_set, col):
        """ Make the specified column numeric. If non-numeric values exist in this column, an error will result.
        """
        col = self.resolve_column(col)
        for row in data_set:
            row[col] = float(row[col])

    def norm_col_range(self, data_set, col, normalized_low, normalized_high):
        """ Perform range normalization on the specified column.  The min/max will be calculated for the column and
            all values will be normalized to the requested map.
        """
        col = self.resolve_column(col)

        # Obtain the high and low values for the column.
        data_low = self.min(data_set, col)
        data_high = self.max(data_set, col)

        # Iterate over all rows and perform the normalization.
        for row in data_set:
            row[col] = ((row[col] - data_low) / (data_high - data_low)) \
                * (normalized_high - normalized_low) + normalized_low

    def build_class_map(self, data_set, col):
        """ Build a class map.  Return a dictionary that contains a mapping between each unique class in the specified
            column and that class's assigned index.  This is used to perform both one-of-n and equilateral encoding.
        """
        col = self.resolve_column(col)
        result = {}
        index = 0
        for row in data_set:
            key = row[col]
            if key not in result:
                result[key] = index
                index += 1
        return result

    def norm_col_one_of_n(self, data_set, col, classes, normalized_low, normalized_high):
        """ Normalize a column using one-of-n.  The classes parameter contains a map of the unique items in the
            specified column.  Typically this value is obtained by calling build_class_map.
        """
        col = self.resolve_column(col)

        for row in data_set:
            key = row[col]
            value = classes[key]
            row.pop(col)
            for i in range(0, len(classes)):
                if i == value:
                    row.insert(col + i, normalized_high)
                else:
                    row.insert(col + i, normalized_low)

    def denorm_one_of_n(self, data):
        """ Denormalize a single value, using one-of-n.
        @param data: The data to denormalize.
        @return: The index of the highest value
        """
        max_value = 0
        max_index = -1

        for i in range(0, len(data)):
            if max_index == -1 or data[i] > max_value:
                max_value = data[i]
                max_index = i

        return max_index

    def norm_col_equilateral(self, data_set, col, classes, normalized_low, normalized_high):
        """ Normalize a column using equilateral.  The classes parameter contains a map of the unique items in the
            specified column.  Typically this value is obtained by calling build_class_map.
        """
        col = self.resolve_column(col)
        eq = Equilateral(len(classes), normalized_low, normalized_high)

        for row in data_set:
            key = row[col]
            value = classes[key]
            row.pop(col)
            vec = eq.encode(value)
            for i in range(0, len(vec)):
                row.insert(col + i, vec[i])

    def resolve_column(self, col):
        """ Resolve a column to an index.  If the value is numeric then this value will be checked to make sure it is
            a valid column index.  If the column index is invalid, an error will occur. If the column is text, then
            we check to see if it matches one of the headers.  If no match can be found, then an error results.
        """
        if type(col) is int:
            # Handle an integer index.
            if col < 0 or col >= len(self.column_map):
                raise AIFHError("Column index out of range: " + str(col))
            return col
        else:
            # Handle a string column name.
            if col not in self.column_map:
                raise AIFHError("Undefined column: " + col)
            else:
                return self.column_map[col]

    def col_extract(self, data_set, col):
        result = []
        col = self.resolve_column(col)
        for row in data_set:
            result.append(row[col])
        return result

    def delete_unknowns(self, data_set):
        """ Delete unknown data, any row that has one or more ? columns.
        @param data_set: The data set.
        """
        i = 0
        while i < len(data_set):
            row = data_set[i]
            for col_data in row:
                if col_data == "?":
                    del data_set[i]
                    break
            i += 1

    def col_delete(self, data_set, col):
        """ Delete the specified column.
        @param data_set: The data set to delete from.
        @param col: The column to delete.
        """
        col = self.resolve_column(col)
        for row in data_set:
            del row[col]

    def col_replace(self, data_set, col, search_for, replace_with, others):
        """ Replace values in the specified column.
        @param data_set: The data set.
        @param col: The column to replace.
        @param search_for: The value we seek to replace.
        @param replace_with: What to replace the specified value with.
        @param others: What to set other values to.
        """
        for row in data_set:
            d = float(row[col])
            if np.abs(d - search_for) < 0.0001:
                row[col] = float(replace_with)
            else:
                row[col] = float(others)
