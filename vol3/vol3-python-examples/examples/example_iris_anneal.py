import numpy as np
import matplotlib.pyplot as plt
from lib.aifh.util import *
import types
from sklearn import svm, datasets
import sklearn
import scipy.stats
import numpy as np
from lasagne.layers import DenseLayer
from lasagne.layers import InputLayer
from lasagne.nonlinearities import sigmoid
from lasagne.nonlinearities import softmax
from lasagne.nonlinearities import rectify
from lasagne.updates import nesterov_momentum
from nolearn.lasagne import NeuralNet

# Compute a z-score with a mean and standard deviation different than the provided matrix.
def zscore(x,mean,sdev):
    return (x-mean)/sdev

def extract_weights(net):
    result = None
    weights = net.get_all_params_values()
    for key in weights:
        for a in weights[key]:
            for b in a:
                if result is None:
                    result = b
                else:
                    result = np.hstack( [result,b] )
    return result

# Define the structure of the neural network
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
    max_epochs=100000,

    on_epoch_finished=[
        EarlyStopping(patience=20)
    ]
)

# Get the iris dataset from scipy
iris = datasets.load_iris()

# Split the iris dataset into 25% validation, and 75% train.  Also shuffle with a seed of 42.
X_train, X_validate, y_train, y_validate = sklearn.cross_validation.train_test_split(
    iris.data,iris.target, test_size = 0.25, random_state = 42)

# Calculate the mean and standard deviation vectors (all 4 measurements) for training data.
train_mean = np.mean(X_train, axis=0)
train_sdev = np.std(X_train, axis=0)

# Compute the z-scores for both train and validation.  However, use mean and standard deviation for training
# on both.  This is customary because we trained on this standard deviation and mean.  Additionally, our
# prediction set might too small to calculate a meaningful mean and standard deviation.
X_train_z = zscore(X_train, train_mean, train_sdev) #scipy.stats.mstats.zscore(X_train)
X_validate_z = zscore(X_validate, train_mean, train_sdev)  #scipy.stats.mstats.zscore(X_validate)

#These can be used to check my zscore calc to numpy
#print(X_train_z)
#print(scipy.stats.mstats.zscore(X_train))

# Provide our own validation set
def my_split(self, X, y, eval_size):
    return X_train_z,X_validate_z,y_train,y_validate

net0.train_test_split = types.MethodType(my_split, net0)

# Train the network
net0.initialize()
d = extract_weights(net0)
print("D:" + str(len(d)))

#net0.fit(X_train_z,y_train)

# Predict the validation set
pred_y = net0.predict(X_validate_z)

# Display predictions and count the number of incorrect predictions.
species_names = ['setosa','versicolour','virginica']

count = 0
wrong = 0
for element in zip(X_validate,y_validate,pred_y):
    print("Input: sepal length: {}, sepal width: {}, petal length: {}, petal width: {}; Expected: {}; Actual: {}".format(
        element[0][0],element[0][1],element[0][2],element[0][3],
        species_names[element[1]],
        species_names[element[2]]))
    if element[1] != element[2]:
        wrong = wrong + 1
    count = count + 1

print("Incorrect {}/{} ({}%)".format(wrong,count,(wrong/count)*100))

