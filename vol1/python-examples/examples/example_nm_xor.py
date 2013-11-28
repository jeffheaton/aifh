__author__ = 'jheaton'

import os
import sys
import numpy as np
from scipy.optimize import minimize, rosen, rosen_der

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from normalize import Normalize
from rbf_network import RbfNetwork
from error import ErrorCalculation


training_input = np.array([
    [0.0,0.0],
    [1.0,0.0],
    [0.0,1.0],
    [1.0,1.0]])

training_ideal = np.array([
    [0.0],
    [1.0],
    [1.0],
    [0.0],
    ])


network = RbfNetwork(2,5,1)
network.reset()

def score_funct(x):
    network.copy_memory(x)
    actual_output = []
    for input_data in training_input:
        output_data = network.compute_regression(input_data)
        actual_output.append(output_data)
    return ErrorCalculation.mse(np.array(actual_output),training_ideal)

#print(score_funct(network.longTermMemory))
x0 = network.longTermMemory[:]
print(score_funct(x0))
res = minimize(score_funct, x0, method='nelder-mead', options={'disp': True, 'maxiter' : 10000})
print(score_funct(res.x))
print(res.x)

for input_data in training_input:
        output_data = network.compute_regression(input_data)
        print(str(input_data) + " -> " + str(output_data))