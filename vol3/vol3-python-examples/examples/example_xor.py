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
import types
import numpy as np
from lasagne.layers import DenseLayer
from lasagne.layers import InputLayer
from lasagne.nonlinearities import sigmoid
from lasagne.nonlinearities import rectify
from lasagne.updates import nesterov_momentum
from nolearn.lasagne import NeuralNet

layers0 = [('input', InputLayer),
           ('dense0', DenseLayer),
           ('output', DenseLayer)]

net0 = NeuralNet(layers=layers0,
    input_shape=(None, 2),
    dense0_num_units=5,
    dense0_nonlinearity = sigmoid,
    output_num_units=1,
    output_nonlinearity=sigmoid,

    update=nesterov_momentum,
    update_learning_rate=0.5,
    update_momentum=0.9,
    regression=True,

    eval_size=None,
    verbose=1,
    max_epochs=200)


X = np.array([ [0,0], [0,1], [1,0], [1,1] ])
y = np.array([ [0.0], [1.0], [1.0], [0.0] ], dtype=np.float32)

def my_split(self, X, y, eval_size):
    return X,X,y,y

net0.train_test_split = types.MethodType(my_split, net0)

net0.fit(X,y)

pred_y = net0.predict(X)

for element in zip(X,y,pred_y):
    print("Input: {}, Ideal: {}, Actual: {}".format(element[0],element[1],element[2]))
