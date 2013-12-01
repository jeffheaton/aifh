#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 1: Fundamental Algorithms
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com

    Code repository:
    https://github.com/jeffheaton/aifh

    Copyright 2013 by Jeff Heaton

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
__author__ = 'jheaton'

import os
import sys
import pandas as pd
import statsmodels.api as sm

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from normalize import Normalize


# find the Wisconsin breast cancer data set
dataFile = os.path.dirname(os.path.realpath(__file__))
dataFile = os.path.abspath(dataFile + "../../datasets/breast-cancer-wisconsin.csv")

# Normalize the Wisconsin file.

norm = Normalize()
data_file_work = norm.load_csv(dataFile)
norm.delete_unknowns(data_file_work)
norm.col_delete(data_file_work, 0)
norm.col_replace(data_file_work, 9, 4, 1, 0)

for i in xrange(0, 9):
    norm.make_col_numeric(data_file_work, i)

print(data_file_work)

#training = np.array(data_file_work)
#training_input = training[:, 0:9]
#training_ideal = training[:, 9:10]

#df = pd.read_csv(dataFile)
#df = df[df.line_race != 0]

df = pd.DataFrame(data_file_work)
df.columns = ["clump_thickness", "size_uniformity", "shape_uniformity", "marginal_adhesion", "epithelial_size",
              "bare_nucleoli", "bland_chromatin", "normal_nucleoli", "mitoses", "class"]

print(df)

train_cols = df.columns[0:9]
print(train_cols)

logit = sm.Logit(df['class'], df[train_cols])

# fit the model
result = logit.fit()

print(result.summary())