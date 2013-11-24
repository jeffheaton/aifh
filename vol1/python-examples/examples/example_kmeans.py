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
classes = norm.col_extract(iris_data,4)
norm.col_delete(iris_data,4)
for i in range(0,4):
    norm.make_col_numeric(iris_data,i)

# Cluster the Iris data set.
res, idx = kmeans2(np.array(iris_data),k)

for cluster_num in range(0,k) :
    print( "Cluster #" + str(cluster_num+1))
    for i in range(0,len(idx)) :
        if idx[i] == cluster_num :
            print( str(iris_data[i]) + "," + classes[i])
