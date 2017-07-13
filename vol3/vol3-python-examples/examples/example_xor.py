#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 3: Deep Learning and Neural Networks
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com
    Code repository:
    https://github.com/jeffheaton/aifh
    Copyright 2017 by Jeff Heaton
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


    This program trains a neural network to be an XOR operator.
    This is a random process, so if you do not get results similar to below
    just rerun.  The actual output from the neural network is not expected
    to exactly equal the ideal.

    Output should be similar to:

    Input: [ 0.  0.], Ideal: [ 0.], Actual: [ 0.01280871]
    Input: [ 0.  1.], Ideal: [ 1.], Actual: [ 0.98295271]
    Input: [ 1.  0.], Ideal: [ 1.], Actual: [ 0.98494756]
    Input: [ 1.  1.], Ideal: [ 0.], Actual: [ 0.02071664]
"""
import types
import numpy as np
from keras.models import Sequential
from keras.layers.core import Dense, Activation
from keras.utils import np_utils


x = np.array([ [0,0], [0,1], [1,0], [1,1] ], dtype=np.float32)
y = np.array([ [0.0], [1.0], [1.0], [0.0] ], dtype=np.float32)

net = Sequential()
net.add(Dense(5, input_shape=(x.shape[1],)))
net.add(Activation('tanh'))
net.add(Dense(1))
net.compile(loss='mean_squared_error', optimizer='adam')

net.fit(x, y, verbose=1, batch_size=4,epochs=1000)

pred_y = net.predict(x)

for element in zip(x,y,pred_y):
    print("Input: {}, Ideal: {}, Actual: {}".format(element[0],element[1],element[2]))
