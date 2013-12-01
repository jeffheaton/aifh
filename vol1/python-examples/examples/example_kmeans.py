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

import os
import sys
from scipy.cluster.vq import *
import numpy as np

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from normalize import Normalize

k = 3

# find the Iris data set
irisFile = os.path.dirname(os.path.realpath(__file__))
irisFile = os.path.abspath(irisFile + "../../datasets/iris.csv")

# Read the Iris data set.
print('Reading CSV file: ' + irisFile)
norm = Normalize()
iris_data = norm.load_csv(irisFile)

# Prepare the iris data set.
classes = norm.col_extract(iris_data, 4)
norm.col_delete(iris_data, 4)
for i in range(0, 4):
    norm.make_col_numeric(iris_data, i)

# Cluster the Iris data set.
res, idx = kmeans2(np.array(iris_data), k)

for cluster_num in range(0, k):
    print( "Cluster #" + str(cluster_num + 1))
    for i in range(0, len(idx)):
        if idx[i] == cluster_num:
            print( str(iris_data[i]) + "," + classes[i])
