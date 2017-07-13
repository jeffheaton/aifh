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
from sklearn import preprocessing
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import shutil
import os


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


# Encode text values to dummy variables(i.e. [1,0,0],[0,1,0],[0,0,1] for red,green,blue)
def encode_text_dummy(df, name):
    dummies = pd.get_dummies(df[name])
    for x in dummies.columns:
        dummy_name = "{}-{}".format(name, x)
        df[dummy_name] = dummies[x]
    df.drop(name, axis=1, inplace=True)


# Encode text values to a single dummy variable.  The new columns (which do not replace the old) will have a 1
# at every location where the original column (name) matches each of the target_values.  One column is added for
# each target value.
def encode_text_single_dummy(df, name, target_values):
    for tv in target_values:
        l = list(df[name].astype(str))
        l = [1 if str(x) == str(tv) else 0 for x in l]
        name2 = "{}-{}".format(name, tv)
        df[name2] = l


# Encode text values to indexes(i.e. [1],[2],[3] for red,green,blue).
def encode_text_index(df, name):
    le = preprocessing.LabelEncoder()
    df[name] = le.fit_transform(df[name])
    return le.classes_


# Encode a numeric column as zscores
def encode_numeric_zscore(df, name, mean=None, sd=None):
    if mean is None:
        mean = df[name].mean()

    if sd is None:
        sd = df[name].std()

    df[name] = (df[name] - mean) / sd


# Convert all missing values in the specified column to the median
def missing_median(df, name):
    med = df[name].median()
    df[name] = df[name].fillna(med)


# Convert all missing values in the specified column to the default
def missing_default(df, name, default_value):
    df[name] = df[name].fillna(default_value)


# Convert a Pandas dataframe to the x,y inputs that TensorFlow needs
def to_xy(df, target):
    result = []
    for x in df.columns:
        if x != target:
            result.append(x)

    # find out the type of the target column.  Is it really this hard? :(
    target_type = df[target].dtypes
    target_type = target_type[0] if hasattr(target_type, '__iter__') else target_type

    # Encode to int for classification, float otherwise. TensorFlow likes 32 bits.
    if target_type in (np.int64, np.int32):
        # Classification
        dummies = pd.get_dummies(df[target])
        return df.as_matrix(result).astype(np.float32), dummies.as_matrix().astype(np.float32)
    else:
        # Regression
        return df.as_matrix(result).astype(np.float32), df.as_matrix([target]).astype(np.float32)


# Nicely formatted time string
def hms_string(sec_elapsed):
    h = int(sec_elapsed / (60 * 60))
    m = int((sec_elapsed % (60 * 60)) / 60)
    s = sec_elapsed % 60
    return "{}:{:>02}:{:>05.2f}".format(h, m, s)


# Regression chart, we will see more of this chart in the next class.
def chart_regression(pred, y):
    t = pd.DataFrame({'pred': pred, 'y': y.flatten()})
    t.sort_values(by=['y'], inplace=True)
    a = plt.plot(t['y'].tolist(), label='expected')
    b = plt.plot(t['pred'].tolist(), label='prediction')
    plt.ylabel('output')
    plt.legend()
    plt.show()


# Get a new directory to hold checkpoints from a neural network.  This allows the neural network to be
# loaded later.  If the erase param is set to true, the contents of the directory will be cleared.
def get_model_dir(name, erase):
    base_path = os.path.join(".", "dnn")
    model_dir = os.path.join(base_path, name)
    os.makedirs(model_dir, exist_ok=True)
    if erase and len(model_dir) > 4 and os.path.isdir(model_dir):
        shutil.rmtree(model_dir, ignore_errors=True)  # be careful, this deletes everything below the specified path
    return model_dir


# Remove all rows where the specified column is +/- sd standard deviations
def remove_outliers(df, name, sd):
    drop_rows = df.index[(np.abs(df[name] - df[name].mean()) >= (sd * df[name].std()))]
    df.drop(drop_rows, axis=0, inplace=True)


# Encode a column to a range between normalized_low and normalized_high.
def encode_numeric_range(df, name, normalized_low=-1, normalized_high=1,
                         data_low=None, data_high=None):
    if data_low is None:
        data_low = min(df[name])
        data_high = max(df[name])

    df[name] = ((df[name] - data_low) / (data_high - data_low)) \
               * (normalized_high - normalized_low) + normalized_low