#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 2: Nature-Inspired Algorithms
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com

    Code repository:
    https://github.com/jeffheaton/aifh

    Copyright 2014 by Jeff Heaton

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
    This example takes awhile to execute.  It uses a genetic algorithm to fit an RBF network to the iris data set.
    You can see the output from the example here.  As you can see, it took 58 iterations to train to 0.05.
    You can see that it is able to classify many of the iris species correctly, but not all.

    This example uses one-of-n encoding for the iris species.  Equilateral could have also been used.

Generaton #1, Score=0.199843346838, stagnant=0
Generaton #2, Score=0.199843346838, stagnant=0
Generaton #3, Score=0.193606061977, stagnant=1
Generaton #4, Score=0.182932591913, stagnant=0
Generaton #5, Score=0.165157776619, stagnant=0
Generaton #6, Score=0.15796529294, stagnant=0
Generaton #7, Score=0.157826592807, stagnant=0
Generaton #8, Score=0.149478480898, stagnant=1
Generaton #9, Score=0.142609733514, stagnant=0
Generaton #10, Score=0.141267076301, stagnant=0
Generaton #11, Score=0.13387570015, stagnant=0
Generaton #12, Score=0.131977908763, stagnant=0
Generaton #13, Score=0.126539359115, stagnant=0
Generaton #14, Score=0.122389808687, stagnant=0
Generaton #15, Score=0.121392668139, stagnant=0
Generaton #16, Score=0.11318352856, stagnant=1
Generaton #17, Score=0.111552631929, stagnant=0
Generaton #18, Score=0.104332331742, stagnant=0
Generaton #19, Score=0.103101332438, stagnant=0
Generaton #20, Score=0.100584671844, stagnant=0
Generaton #21, Score=0.0974004283988, stagnant=0
Generaton #22, Score=0.094533902446, stagnant=0
Generaton #23, Score=0.0910003821609, stagnant=0
Generaton #24, Score=0.0910003821609, stagnant=0
Generaton #25, Score=0.0905620576106, stagnant=1
Generaton #26, Score=0.0866654176526, stagnant=2
Generaton #27, Score=0.0826733880209, stagnant=0
Generaton #28, Score=0.0816455270936, stagnant=0
Generaton #29, Score=0.0799649368276, stagnant=0
Generaton #30, Score=0.0797301141794, stagnant=0
Generaton #31, Score=0.0774793573792, stagnant=1
Generaton #32, Score=0.0767527501314, stagnant=0
Generaton #33, Score=0.0764559059563, stagnant=1
Generaton #34, Score=0.0749918540669, stagnant=2
Generaton #35, Score=0.0723100319898, stagnant=0
Generaton #36, Score=0.071279017377, stagnant=0
Generaton #37, Score=0.0692806352376, stagnant=0
Generaton #38, Score=0.0687199631007, stagnant=0
Generaton #39, Score=0.0671800095714, stagnant=1
Generaton #40, Score=0.0651154796387, stagnant=0
Generaton #41, Score=0.0640848760543, stagnant=0
Generaton #42, Score=0.062768548122, stagnant=0
Generaton #43, Score=0.0623897612924, stagnant=0
Generaton #44, Score=0.0613174410677, stagnant=1
Generaton #45, Score=0.0600323016682, stagnant=0
Generaton #46, Score=0.0590140769361, stagnant=0
Generaton #47, Score=0.0579662753868, stagnant=0
Generaton #48, Score=0.0563771595186, stagnant=0
Generaton #49, Score=0.0557091224927, stagnant=0
Generaton #50, Score=0.0557091224927, stagnant=1
Generaton #51, Score=0.0556228207268, stagnant=2
Generaton #52, Score=0.0547559332724, stagnant=3
Generaton #53, Score=0.0547559332724, stagnant=4
Generaton #54, Score=0.0544944263627, stagnant=5
Generaton #55, Score=0.0539352236468, stagnant=6
Generaton #56, Score=0.0535581096618, stagnant=7
Generaton #57, Score=0.0527253713172, stagnant=8
Generaton #58, Score=0.0525153691128, stagnant=9
[ 0.22222222  0.625       0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.16666667  0.41666667  0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.11111111  0.5         0.05084746  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.08333333  0.45833333  0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.66666667  0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.30555556  0.79166667  0.11864407  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.08333333  0.58333333  0.06779661  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.58333333  0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.02777778  0.375       0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.16666667  0.45833333  0.08474576  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.30555556  0.70833333  0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.13888889  0.58333333  0.10169492  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.13888889  0.41666667  0.06779661  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.          0.41666667  0.01694915  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.41666667  0.83333333  0.03389831  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.38888889  1.          0.08474576  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.30555556  0.79166667  0.05084746  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.625       0.06779661  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.38888889  0.75        0.11864407  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.75        0.08474576  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.30555556  0.58333333  0.11864407  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.70833333  0.08474576  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.08333333  0.66666667  0.          0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.54166667  0.11864407  0.16666667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.13888889  0.58333333  0.15254237  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.41666667  0.10169492  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.58333333  0.10169492  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.25        0.625       0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.25        0.58333333  0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.11111111  0.5         0.10169492  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.13888889  0.45833333  0.10169492  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.30555556  0.58333333  0.08474576  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.25        0.875       0.08474576  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.33333333  0.91666667  0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.16666667  0.45833333  0.08474576  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.5         0.03389831  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.33333333  0.625       0.05084746  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.16666667  0.45833333  0.08474576  0.        ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.02777778  0.41666667  0.05084746  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.58333333  0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.625       0.05084746  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.05555556  0.125       0.05084746  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.02777778  0.5         0.05084746  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.625       0.10169492  0.20833333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.75        0.15254237  0.125     ] -> Iris-setosa, Ideal: Iris-setosa
[ 0.13888889  0.41666667  0.06779661  0.08333333] -> Iris-setosa, Ideal: Iris-setosa
[ 0.22222222  0.75        0.10169492  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.08333333  0.5         0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.27777778  0.70833333  0.08474576  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.19444444  0.54166667  0.06779661  0.04166667] -> Iris-setosa, Ideal: Iris-setosa
[ 0.75        0.5         0.62711864  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.58333333  0.5         0.59322034  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.72222222  0.45833333  0.66101695  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.33333333  0.125       0.50847458  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.61111111  0.33333333  0.61016949  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.38888889  0.33333333  0.59322034  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.55555556  0.54166667  0.62711864  0.625     ] -> Iris-virginica, Ideal: Iris-versicolor
[ 0.16666667  0.16666667  0.38983051  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.63888889  0.375       0.61016949  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.25        0.29166667  0.49152542  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.19444444  0.          0.42372881  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.44444444  0.41666667  0.54237288  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.47222222  0.08333333  0.50847458  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.5         0.375       0.62711864  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.36111111  0.375       0.44067797  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.66666667  0.45833333  0.57627119  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.36111111  0.41666667  0.59322034  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.41666667  0.29166667  0.52542373  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.52777778  0.08333333  0.59322034  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.36111111  0.20833333  0.49152542  0.41666667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.44444444  0.5         0.6440678   0.70833333] -> Iris-virginica, Ideal: Iris-versicolor
[ 0.5         0.33333333  0.50847458  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.55555556  0.20833333  0.66101695  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.5         0.33333333  0.62711864  0.45833333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.58333333  0.375       0.55932203  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.63888889  0.41666667  0.57627119  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.69444444  0.33333333  0.6440678   0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.66666667  0.41666667  0.6779661   0.66666667] -> Iris-virginica, Ideal: Iris-versicolor
[ 0.47222222  0.375       0.59322034  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.38888889  0.25        0.42372881  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.33333333  0.16666667  0.47457627  0.41666667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.33333333  0.16666667  0.45762712  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.41666667  0.29166667  0.49152542  0.45833333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.47222222  0.29166667  0.69491525  0.625     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.30555556  0.41666667  0.59322034  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.47222222  0.58333333  0.59322034  0.625     ] -> Iris-virginica, Ideal: Iris-versicolor
[ 0.66666667  0.45833333  0.62711864  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.55555556  0.125       0.57627119  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.36111111  0.41666667  0.52542373  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.33333333  0.20833333  0.50847458  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.33333333  0.25        0.57627119  0.45833333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.5         0.41666667  0.61016949  0.54166667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.41666667  0.25        0.50847458  0.45833333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.19444444  0.125       0.38983051  0.375     ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.36111111  0.29166667  0.54237288  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.38888889  0.41666667  0.54237288  0.45833333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.38888889  0.375       0.54237288  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.52777778  0.375       0.55932203  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.22222222  0.20833333  0.33898305  0.41666667] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.38888889  0.33333333  0.52542373  0.5       ] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.55555556  0.54166667  0.84745763  1.        ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.41666667  0.29166667  0.69491525  0.75      ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.77777778  0.41666667  0.83050847  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.55555556  0.375       0.77966102  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.61111111  0.41666667  0.81355932  0.875     ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.91666667  0.41666667  0.94915254  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.16666667  0.20833333  0.59322034  0.66666667] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.83333333  0.375       0.89830508  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.66666667  0.20833333  0.81355932  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.80555556  0.66666667  0.86440678  1.        ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.61111111  0.5         0.69491525  0.79166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.58333333  0.29166667  0.72881356  0.75      ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.69444444  0.41666667  0.76271186  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.38888889  0.20833333  0.6779661   0.79166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.41666667  0.33333333  0.69491525  0.95833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.58333333  0.5         0.72881356  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.61111111  0.41666667  0.76271186  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.94444444  0.75        0.96610169  0.875     ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.94444444  0.25        1.          0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.47222222  0.08333333  0.6779661   0.58333333] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.72222222  0.5         0.79661017  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.36111111  0.33333333  0.66101695  0.79166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.94444444  0.33333333  0.96610169  0.79166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.55555556  0.29166667  0.66101695  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.66666667  0.54166667  0.79661017  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.80555556  0.5         0.84745763  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.52777778  0.33333333  0.6440678   0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.5         0.41666667  0.66101695  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.58333333  0.33333333  0.77966102  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.80555556  0.41666667  0.81355932  0.625     ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.86111111  0.33333333  0.86440678  0.75      ] -> Iris-virginica, Ideal: Iris-virginica
[ 1.          0.75        0.91525424  0.79166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.58333333  0.33333333  0.77966102  0.875     ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.55555556  0.33333333  0.69491525  0.58333333] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.5         0.25        0.77966102  0.54166667] -> Iris-versicolor, Ideal: Iris-virginica
[ 0.94444444  0.41666667  0.86440678  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.55555556  0.58333333  0.77966102  0.95833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.58333333  0.45833333  0.76271186  0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.47222222  0.41666667  0.6440678   0.70833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.72222222  0.45833333  0.74576271  0.83333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.66666667  0.45833333  0.77966102  0.95833333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.72222222  0.45833333  0.69491525  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.41666667  0.29166667  0.69491525  0.75      ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.69444444  0.5         0.83050847  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.66666667  0.54166667  0.79661017  1.        ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.66666667  0.41666667  0.71186441  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.55555556  0.20833333  0.6779661   0.75      ] -> Iris-virginica, Ideal: Iris-virginica
[ 0.61111111  0.41666667  0.71186441  0.79166667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.52777778  0.58333333  0.74576271  0.91666667] -> Iris-virginica, Ideal: Iris-virginica
[ 0.44444444  0.41666667  0.69491525  0.70833333] -> Iris-virginica, Ideal: Iris-virginica

Process finished with exit code 0

"""
__author__ = 'jheaton'

import os
import sys
import numpy as np

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from normalize import Normalize
from rbf_network import RbfNetwork
from error import ErrorCalculation
from genetic import *
from aco import *

# find the Iris data set
irisFile = os.path.dirname(os.path.realpath(__file__))
irisFile = os.path.abspath(irisFile + "../../datasets/iris.csv")

# Read the Iris data set.
print('Reading CSV file: ' + irisFile)
norm = Normalize()
iris_work = norm.load_csv(irisFile)

# Extract the original iris species so we can display during the final validation.
ideal_species = [row[4] for row in iris_work]

# Setup the first four fields to "range normalize" between -1 and 1.
for i in range(0, 4):
    norm.make_col_numeric(iris_work, i)
    norm.norm_col_range(iris_work, i, 0, 1)

# Discover all of the classes for column #4, the iris species.
classes = norm.build_class_map(iris_work, 4)
inv_classes = {v: k for k, v in classes.items()}

# Normalize iris species using one-of-n.
# We could have used equilateral as well.  For an example of equilateral, see the example_nm_iris example.
norm.norm_col_one_of_n(iris_work, 4, classes, 0, 1)


# Prepare training data.  Separate into input and ideal.
training = np.array(iris_work)
training_input = training[:, 0:4]
training_ideal = training[:, 4:7]

# Create an RBF network.  There are four inputs and two outputs.
# There are also five RBF functions used internally.
# You can experiment with different numbers of internal RBF functions.
# However, the input and output must match the data set.
network = RbfNetwork(4, 4, 3)
network.reset()

def score_funct(x):
    """
    The score function for Iris anneal.
    @param x:
    @return:
    """
    global best_score
    global input_data
    global output_data
    # Update the network's long term memory to the vector we need to score.
    network.copy_memory(x)
    # Loop over the training set and calculate the output for each.
    actual_output = []
    for input_data in training_input:
        output_data = network.compute_regression(input_data)
        actual_output.append(output_data)
    # Calculate the error with MSE.
    result = ErrorCalculation.mse(np.array(actual_output), training_ideal)
    return result

# Perform the annealing
train = ContinuousACO(network.long_term_memory,score_funct,30)
train.display_iteration = True
train.train()

# Display the final validation.  We show all of the iris data as well as the predicted species.
for i in range(0, len(training_input)):
    input_data = training_input[i]
    # Compute the output from the RBF network
    output_data = network.compute_regression(input_data)
    ideal_data = training_ideal[i]
    # Decode the three output neurons into a class number.
    class_id = norm.denorm_one_of_n(output_data)
    print(str(input_data) + " -> " + inv_classes[class_id] + ", Ideal: " + ideal_species[i])