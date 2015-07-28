import numpy as np
import matplotlib.pyplot as plt
from sklearn import svm, datasets
import scipy.stats
from lasagne.layers import DenseLayer
from lasagne.layers import InputLayer
from lasagne.nonlinearities import sigmoid
from lasagne.nonlinearities import softmax
from lasagne.nonlinearities import rectify
from lasagne.updates import nesterov_momentum
from nolearn.lasagne import NeuralNet


iris = datasets.load_iris()


X = iris.data
y = iris.target

X = scipy.stats.mstats.zscore(X)


layers0 = [('input', InputLayer),
           ('dense0', DenseLayer),
           ('output', DenseLayer)]

net0 = NeuralNet(layers=layers0,
    input_shape=(None, 4),
    dense0_num_units=50,
    dense0_nonlinearity = rectify,
    output_num_units=3,
    output_nonlinearity=softmax,

    update=nesterov_momentum,
    update_learning_rate=0.01,
    update_momentum=0.9,
    regression=False,

    eval_size=0.0,
    verbose=1,
    max_epochs=1000)

net0.fit(X,y)
