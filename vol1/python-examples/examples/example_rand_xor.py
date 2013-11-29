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
from train import TrainGreedRandom


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


x0 = network.longTermMemory[:]
print(score_funct(x0))

train = TrainGreedRandom(-1,1)
train.display_iteration = True
train.max_iterations = 500000
train.stop_score = 0.05
train.train(x0,score_funct)

network.copy_memory(train.position)

for input_data in training_input:
        output_data = network.compute_regression(input_data)
        print(str(input_data) + " -> " + str(output_data))