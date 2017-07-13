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

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
data_dir = aifh_dir
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
data_dir = os.path.abspath(data_dir + os.sep + ".." + os.sep + "datasets")
sys.path.append(aifh_dir)

from util import *
import pandas as pd
import types
from sklearn import svm, datasets
from sklearn.model_selection import train_test_split
import sklearn
import numpy as np
from keras.models import Sequential
from keras.layers.core import Dense, Activation
from keras.utils import np_utils
from sklearn.preprocessing import OneHotEncoder
from normalize import *

filename = os.path.join(data_dir, "iris.csv")
df = pd.read_csv(filename, na_values=['NA', '?'])

# Encode feature vector
encode_numeric_zscore(df, 'petal_w')
encode_numeric_zscore(df, 'petal_l')
encode_numeric_zscore(df, 'sepal_w')
encode_numeric_zscore(df, 'sepal_l')
species = encode_text_index(df, "species")
num_classes = len(species)

# Create x & y for training

# Create the x-side (feature vectors) of the training
x, y = to_xy(df, 'species')

# Split into train/test
x_train, x_test, y_train, y_test = train_test_split(
    x, y, test_size=0.25, random_state=45)

net = Sequential()
net.add(Dense(20, input_shape=(x.shape[1],)))
net.add(Activation('relu'))
net.add(Dense(y.shape[1]))
net.add(Activation('softmax'))
net.compile(loss='categorical_crossentropy', optimizer='adam')

# Train the network
net.fit(x_train,y_train,epochs=300)

# Predict the validation set
pred_y = net.predict(x_test)
pred_y = np.argmax(pred_y,axis=1)
y_test2 = np.argmax(y_test,axis=1)
print(pred_y)
print(y_test2)


# Display predictions and count the number of incorrect predictions.

count = 0
correct = 0
for element in zip(x_test,y_test2,pred_y):
    print(element)
    print("Input: sepal length: {}, sepal width: {}, petal length: {}, petal width: {}; Expected: {}; Actual: {}".format(
        element[0][0],element[0][1],element[0][2],element[0][3],
        species[element[1]],
        species[element[2]]))
    if element[1] == element[2]:
        correct += 1
    count += 1

print("Correct: {}/{} ({}%)".format(correct,count,(correct/count)*100))

