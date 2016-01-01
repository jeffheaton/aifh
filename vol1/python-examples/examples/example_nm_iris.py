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
This example makes use of the scipy minimize function's Nelder Mead optimization to fit an RBF network to
the Iris data set.  The minimize function does not provide any means of getting iteration updates.  As a result,
we simply display the score each time the score function finds a new "best score".  This is why we only see score
output while training.  The output is shown here.

This example uses equilateral encoding for the iris species.  One-of-n could have also been used.

Score: 0.876607540507
Score: 0.872947793775
Score: 0.872773114372
Score: 0.869203771704
Score: 0.845630327976
Score: 0.830420339364
Score: 0.783792972766
Score: 0.783697863852
Score: 0.754818896444
Score: 0.754701099429
Score: 0.727019764856
Score: 0.726388831873
Score: 0.701715519446
Score: 0.698862208221
Score: 0.663964159335
Score: 0.663446570122
...
Score: 0.0133080614936
Score: 0.0132966526154
Score: 0.0132610347522
Score: 0.013256043042
Score: 0.0132355504628
Score: 0.0132202604192
Score: 0.0132141283541
Score: 0.0132137709576
Warning: Maximum number of iterations has been exceeded.
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
[ 0.55555556  0.54166667  0.62711864  0.625     ] -> Iris-versicolor, Ideal: Iris-versicolor
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
[ 0.47222222  0.29166667  0.69491525  0.625     ] -> Iris-virginica, Ideal: Iris-versicolor
[ 0.30555556  0.41666667  0.59322034  0.58333333] -> Iris-versicolor, Ideal: Iris-versicolor
[ 0.47222222  0.58333333  0.59322034  0.625     ] -> Iris-versicolor, Ideal: Iris-versicolor
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
[ 0.55555556  0.33333333  0.69491525  0.58333333] -> Iris-virginica, Ideal: Iris-virginica
[ 0.5         0.25        0.77966102  0.54166667] -> Iris-virginica, Ideal: Iris-virginica
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
from scipy.optimize import minimize

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from normalize import Normalize
from rbf_network import RbfNetwork
from error import ErrorCalculation
from equilateral import Equilateral


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

# Normalize iris species using equilateral
norm.norm_col_equilateral(iris_work, 4, classes, 0, 1)


# Display the resulting data
#norm.display_data(result)

training = np.array(iris_work)
training_input = training[:, 0:4]
training_ideal = training[:, 4:6]

# Create an RBF network.  There are four inputs and two outputs.
# There are also five RBF functions used internally.
# You can experiment with different numbers of internal RBF functions.
# However, the input and output must match the data set.
network = RbfNetwork(4, 5, 2)
network.reset()

best_score = sys.float_info.max


def score_funct(x):
    global best_score
    global input_data
    global output_data
    network.copy_memory(x)
    actual_output = []
    for input_data in training_input:
        output_data = network.compute_regression(input_data)
        actual_output.append(output_data)

    result = ErrorCalculation.mse(np.array(actual_output), training_ideal)
    if result < best_score:
        best_score = result
        print("Score: " + str(result))
    return result

# Initial state is current long term memory.
x0 = list(network.long_term_memory)

# Train the network.
res = minimize(score_funct, x0, method='nelder-mead', tol=0.0001, options={'disp': True, 'maxiter': 5000})

# Create an equilateral table for 3 classes (species of iris) and between the range 0 to 1.  This is used
# to decide the two output nodes into a species number.
eq = Equilateral(3, 0, 1)

# Display the final validation.  We show all of the iris data as well as the predicted species.
for i in range(0, len(training_input)):
    input_data = training_input[i]
    # Compute the output from the RBF network
    output_data = network.compute_regression(input_data)
    ideal_data = training_ideal[i]
    # Decode the two output neurons into a class number.
    class_id = eq.decode(output_data)
    print(str(input_data) + " -> " + inv_classes[class_id] + ", Ideal: " + ideal_species[i])
