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
import lasagne
from lib.aifh.mnist import *
from lasagne.objectives import mse
import theano
import theano.tensor as T
import time

num_epochs = 50

def build_mlp(input_var=None):
    # Shape is: no predefined batch size,
    l_in = lasagne.layers.InputLayer(shape=(None, 2),
                                     input_var=input_var)

    l_hid1 = lasagne.layers.DenseLayer(
            l_in, num_units=5,
            nonlinearity=lasagne.nonlinearities.sigmoid,
            W=lasagne.init.GlorotUniform())

    l_out = lasagne.layers.DenseLayer(
            l_hid1, num_units=2,
            nonlinearity=lasagne.nonlinearities.sigmoid)

    return l_out


X_train = [ [0,0], [0,1], [1,0], [1,1] ]
y_train = [ [0.0], [1.0], [1.0], [0.0] ]


# Prepare Theano variables for inputs and targets
input_var_type = T.TensorType(theano.config.floatX, [False] * 2)
input_var = input_var_type("inputs")
target_var = T.fmatrix('targets')

network = build_mlp(input_var)

prediction = lasagne.layers.get_output(network)
loss = mse(prediction, target_var) #lasagne.objectives.binary_crossentropy(prediction, target_var)
loss = loss.mean()

params = lasagne.layers.get_all_params(network, trainable=True)
updates = lasagne.updates.nesterov_momentum(
        loss, params, learning_rate=0.01, momentum=0.9)

train_fn = theano.function([input_var, target_var], loss, updates=updates)

print("Starting training...")

# We iterate over epochs:
for epoch in range(num_epochs):
    # In each epoch, we do a full pass over the training data:
    train_err = 0
    train_batches = 0
    start_time = time.time()
    train_err += train_fn(X_train, y_train)

    print("Epoch {} of {} took {:.3f}s".format(
        epoch + 1, num_epochs, time.time() - start_time))
    print("  training loss:\t\t{:.6f}".format(train_err / train_batches))

