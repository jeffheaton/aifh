import numpy as np
import pandas as pd
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import StandardScaler

from lasagne.layers import DenseLayer
from lasagne.layers import InputLayer
from lasagne.layers import DropoutLayer
from lasagne.nonlinearities import softmax
from lasagne.nonlinearities import sigmoid
from lasagne.updates import nesterov_momentum
from nolearn.lasagne import NeuralNet

layers0 = [('input', InputLayer),
           ('dense0', DenseLayer),
           ('output', DenseLayer)]

net0 = NeuralNet(layers=layers0,
    input_shape=(None, 2),
    dense0_num_units=5,
    output_num_units=1,
    output_nonlinearity=sigmoid,

    update=nesterov_momentum,
    update_learning_rate=0.01,
    update_momentum=0.9,
    regression=True,

    eval_size=0.0,
    verbose=1,
    max_epochs=1000)

X = [ [0,0], [0,1], [1,0], [1,1] ]
y = [ [0.0], [1.0], [1.0], [0.0] ]

net0.fit(X,y)
print(net0.predict(X))
