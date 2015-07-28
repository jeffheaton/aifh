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
import numpy as np
import os
import urllib
import gzip
from lib.aifh.util import *

MNIST_PKL_URL = 'http://deeplearning.net/data/mnist/mnist.pkl.gz'
MNIST_PKL_FILENAME = 'mnist.pkl.gz'

def load_dataset(reshape_data):

    if not os.path.exists(MNIST_PKL_FILENAME):
        print("Downloading MNIST dataset...")
        urllib.request.urlretrieve(MNIST_PKL_URL, MNIST_PKL_FILENAME)

    with gzip.open(MNIST_PKL_FILENAME, 'rb') as f:
        data = pickle_load(f, encoding='latin-1')

    X_train, y_train = data[0]
    X_val, y_val = data[1]
    X_test, y_test = data[2]

    if reshape_data:
        X_train = X_train.reshape((-1, 1, 28, 28))
        X_val = X_val.reshape((-1, 1, 28, 28))
        X_test = X_test.reshape((-1, 1, 28, 28))

    # Cast to int8 for GPU compatibility.
    y_train = y_train.astype(np.uint8)
    y_val = y_val.astype(np.uint8)
    y_test = y_test.astype(np.uint8)

    return X_train, y_train, X_val, y_val, X_test, y_test

