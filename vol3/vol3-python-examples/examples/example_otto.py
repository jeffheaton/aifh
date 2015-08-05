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
import pandas as pd
from sklearn.preprocessing import LabelEncoder
from lasagne.layers import DenseLayer
from lasagne.layers import InputLayer
from lasagne.layers import DropoutLayer
from lasagne.nonlinearities import softmax
from lasagne.updates import nesterov_momentum
from nolearn.lasagne import NeuralNet
from sklearn.preprocessing import StandardScaler
import scipy
import theano
from lib.aifh.eval import *

class EarlyStopping(object):
    def __init__(self, patience=100):
        self.patience = patience
        self.best_valid = np.inf
        self.best_valid_epoch = 0
        self.best_weights = None

    def __call__(self, nn, train_history):
        current_valid = train_history[-1]['valid_loss']
        current_epoch = train_history[-1]['epoch']
        if current_valid < self.best_valid:
            self.best_valid = current_valid
            self.best_valid_epoch = current_epoch
            self.best_weights = nn.get_all_params_values()
        elif self.best_valid_epoch + self.patience < current_epoch:
            print("Early stopping.")
            print("Best valid loss was {:.6f} at epoch {}.".format(
                self.best_valid, self.best_valid_epoch))
            nn.load_params_from(self.best_weights)
            raise StopIteration()


def float32(k):
    return np.cast['float32'](k)

class AdjustVariable(object):
    def __init__(self, name, start=0.03, stop=0.001):
        self.name = name
        self.start, self.stop = start, stop
        self.ls = None

    def __call__(self, nn, train_history):
        if self.ls is None:
            self.ls = np.linspace(self.start, self.stop, nn.max_epochs)

        epoch = train_history[-1]['epoch']
        new_value = float32(self.ls[epoch - 1])
        getattr(nn, self.name).set_value(new_value)

def load_train_data(path):
    df = pd.read_csv(path)
    X = df.values.copy()
    np.random.shuffle(X)
    X, labels = X[:, 1:-1].astype(np.float32), X[:, -1]
    encoder = LabelEncoder()
    y = encoder.fit_transform(labels).astype(np.int32)
    X = scipy.stats.zscore(X,axis=0)
    scaler = StandardScaler()
    X = scaler.fit_transform(X)
    return X, y, encoder, scaler

def load_val_data(path, scaler, encoder):
    df = pd.read_csv(path)
    X = df.values.copy()
    ids = X[:, 0].astype(str)
    X, labels = X[:, 1:-1].astype(np.float32), X[:, -1]
    y = encoder.transform(labels)
    X = scipy.stats.zscore(X,axis=0)
    X = scaler.transform(X)
    return X, y, ids

def load_test_data(path, scaler):
    df = pd.read_csv(path)
    X = df.values.copy()
    X, ids = X[:, 1:].astype(np.float32), X[:, 0].astype(str)
    X = scipy.stats.zscore(X,axis=0)
    X = scaler.transform(X)
    return X, ids

def make_submission(model_count, y_prob, ids, encoder, name='neural-network-bootstrap.csv'):
    temp = y_prob/model_count

    with open(name, 'w') as f:
        f.write('id,')
        f.write(','.join(encoder.classes_))
        f.write('\n')
        for id, probs in zip(ids, temp):
            probas = ','.join([id] + list(map(str, probs.tolist())))
            f.write(probas)
            f.write('\n')
    print("Wrote submission to file {}.".format(name))


def train_network():
    layers0 = [('input', InputLayer),
               ('dense0', DenseLayer),
               ('dropout0', DropoutLayer),
               ('dense1', DenseLayer),
               ('dropout1', DropoutLayer),
               ('dense2', DenseLayer),
               ('output', DenseLayer)]

    es = EarlyStopping(patience=200)
    net0 = NeuralNet(layers=layers0,
        input_shape=(None, num_features),
        dense0_num_units=256,
        dropout0_p=0.5,
        dense1_num_units=128,
        dropout1_p=0.5,
        dense2_num_units=64,
        output_num_units=num_classes,
        output_nonlinearity=softmax,

        update=nesterov_momentum,
        update_learning_rate=theano.shared(float32(0.01)),
        update_momentum=theano.shared(float32(0.9)),

        eval_size=0.2,
        verbose=1,
        max_epochs=1000,
        on_epoch_finished=[
            AdjustVariable('update_learning_rate', start=0.01, stop=0.0001),
            AdjustVariable('update_momentum', start=0.9, stop=0.999),
            es
            ])

    net0.fit(X, y)
    return (es.best_valid, net0)


X, y, encoder, scaler = load_train_data('data/train.csv')
X_test, ids = load_test_data('data/test.csv', scaler)
X_val, y_val, ids_val = load_val_data('data/split_validate.csv', scaler, encoder)

num_classes = len(encoder.classes_)
num_features = X.shape[1]

total_submit = None
total_validate = None

model_count = 0

while True:
    valid_score, net0 = train_network()

    y_prob = net0.predict_proba(X_test)
    if total_submit == None:
        total_submit = y_prob
    else:
        total_submit+=y_prob


    y_prob2 = net0.predict_proba(X_val)
    err_validate = mlogloss(y_prob2, y_val)
    if total_validate == None:
        total_validate = y_prob2
    else:
        total_validate+=y_prob2

    model_count = model_count + 1
    make_submission(model_count, total_submit, ids, encoder,"las-submit.csv")
    make_submission(model_count, total_validate, ids_val, encoder,"las-val.csv")

    err_total = mlogloss(total_validate/model_count, y_val)

    line = "Bagged LAS model: {}, score: {}, current mlog: {}, bagged mlog: {}".format(model_count,valid_score,err_validate,err_total)
    print(line)


