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
import os
import sys
import scipy

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

import numpy as np
from window import *
import pandas
import scipy.stats as ss
from lib.aifh.util import *
import types
from sklearn import svm, datasets
import sklearn
import numpy as np
from lasagne.layers import DenseLayer
from lasagne.layers import InputLayer
from lasagne.nonlinearities import softmax
from lasagne.nonlinearities import rectify
from lasagne.updates import nesterov_momentum
from nolearn.lasagne import NeuralNet

LAG_SIZE = 5
LEAD_SIZE = 1

# Compute a z-score with a mean and standard deviation different than the provided matrix.
def zscore(x,mean,sdev):
    return (x-mean)/sdev


# Define the structure of the neural network
layers0 = [('input', InputLayer),
           ('dense0', DenseLayer),
           ('output', DenseLayer)]

net0 = NeuralNet(layers=layers0,
    input_shape=(None, LAG_SIZE),
    dense0_num_units=50,
    dense0_nonlinearity = rectify,
    output_num_units=LEAD_SIZE,
    output_nonlinearity=None,

    update=nesterov_momentum,
    update_learning_rate=0.01,
    update_momentum=0.9,
    regression=True,

    eval_size=0.0,
    verbose=1,
    max_epochs=100000,

    on_epoch_finished=[
        EarlyStopping(patience=20)
    ]
)

# Load the Sunspot data, remove year and month.
sunspot_data = pandas.read_csv('sunspots.csv')
sunspot_data = sunspot_data.drop('year',1)
sunspot_data = sunspot_data.drop('month',1)
sunspot_data = sunspot_data.as_matrix()


data, target = encode_timeseries_window(sunspot_data, LAG_SIZE, LEAD_SIZE, [True], [True])

# Split the iris dataset into 25% validation, and 75% train.  Also shuffle with a seed of 42.
X_train, X_validate, y_train, y_validate = sklearn.cross_validation.train_test_split(
    data,target, test_size = 0.25, random_state = 42)

# Calculate the mean and standard deviation vectors for training data.
train_mean = np.mean(X_train, axis=0)
train_sdev = np.std(X_train, axis=0)

# Compute the z-scores for both train and validation.  However, use mean and standard deviation for training
# on both.  This is customary because we trained on this standard deviation and mean.  Additionally, our
# prediction set might too small to calculate a meaningful mean and standard deviation.
X_train_z = zscore(X_train, train_mean, train_sdev) #scipy.stats.mstats.zscore(X_train)
y_train_z = zscore(y_train, train_mean[0], train_sdev[0])
X_validate_z = zscore(X_validate, train_mean, train_sdev)  #scipy.stats.mstats.zscore(X_validate)
y_validate_z = zscore(y_validate, train_mean[0], train_sdev[0])

#These can be used to check my zscore calc to numpy
#print(X_train_z)
#print(scipy.stats.mstats.zscore(X_train))

# Provide our own validation set
def my_split(self, X, y, eval_size):
    print("here")
    return X_train_z,X_validate_z,y_train_z,y_validate_z

net0.train_split = types.MethodType(my_split, net0)

# Train the network
net0.fit(X_train_z,y_train_z)

# Predict the validation set
pred_y = net0.predict(X_validate_z)

for element in zip(X_validate_z,y_validate_z,pred_y):
    print("Input: {}; Expected: {}; Actual: {}; Diff: {}".format(
        element[0],
        element[1],
        element[2],
        element[1]-element[2]))
