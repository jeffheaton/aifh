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
============================================================================================================
This example uses a Generalized Linear Model (GLM), in this case a LOGIT to fit to the Wisconsin Breast Cancer
data set.  The output is shown below.  The solution coefficients are shown.




Optimization terminated successfully.
         Current function value: 0.379649
         Iterations 8
                           Logit Regression Results
==============================================================================
Dep. Variable:                  class   No. Observations:                  683
Model:                          Logit   Df Residuals:                      674
Method:                           MLE   Df Model:                            8
Date:                Tue, 03 Dec 2013   Pseudo R-squ.:                  0.4136
Time:                        05:58:46   Log-Likelihood:                -259.30
converged:                       True   LL-Null:                       -442.18
                                        LLR p-value:                 3.925e-74
=====================================================================================
                        coef    std err          z      P>|z|      [95.0% Conf. Int.]
-------------------------------------------------------------------------------------
clump_thickness      -0.3513      0.058     -6.078      0.000        -0.465    -0.238
size_uniformity       0.9319      0.131      7.116      0.000         0.675     1.189
shape_uniformity      0.1919      0.109      1.756      0.079        -0.022     0.406
marginal_adhesion     0.1487      0.076      1.954      0.051        -0.000     0.298
epithelial_size      -0.7858      0.101     -7.778      0.000        -0.984    -0.588
bare_nucleoli         0.5434      0.063      8.561      0.000         0.419     0.668
bland_chromatin      -0.5484      0.092     -5.940      0.000        -0.729    -0.367
normal_nucleoli       0.3625      0.074      4.929      0.000         0.218     0.507
mitoses              -0.2531      0.088     -2.869      0.004        -0.426    -0.080
=====================================================================================
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

for i in range(0, 9):
    norm.make_col_numeric(data_file_work, i)

df = pd.DataFrame(data_file_work)
df.columns = ["clump_thickness", "size_uniformity", "shape_uniformity", "marginal_adhesion", "epithelial_size",
              "bare_nucleoli", "bland_chromatin", "normal_nucleoli", "mitoses", "class"]

train_cols = df.columns[0:9]

# Perform the logistic regression.
logit = sm.Logit(df['class'], df[train_cols])

# fit the model
result = logit.fit()

# Display the results.
print(result.summary())