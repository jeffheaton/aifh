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

class DeepLayer:
    def __init__(self, owner, input_count, output_count):
        self.input_count = input_count
        self.output_count = output_count
        self.weights = np.zeros([output_count,input_count])
        self.bias = np.zeros([output_count])
        self.owner = owner

    def softmax(self, x):
        self.max = 0.0
        self.sum = 0.0

        for aX in x:
            if max < aX:
                max = aX

        for i in range(len(x)):
            x[i] = np.exp(x[i] - max)
            sum += x[i]

        for i in range(len(x)):
            x[i] /= sum

class HiddenLayer(DeepLayer):
    """
    A hidden layer for a DBNN.  This is based on a restricted Boltzmann machine.
    """
    def __init__(self, owner, input_count, output_count):
        """
        Create a hidden layer for a DBNN.
        :param owner: The DBNN that this layer belongs to.
        :param input_count: The number of visible units, the input.
        :param output_count: The number of hidden units, the output.
        """
        super(HiddenLayer, self).__init__(owner, input_count, output_count)

    def binomial(self,n,p):
        """
        Sample n times at probability p and return the count of how many samples were 1 (true).
        :param n: The number of samples needed.
        :param p: The probability of choosing 1 (true).
        :return: The count of how many 1 (true)'s were sampled.
        """
        if p < 0 or p > 1:
            return 0

        c = 0

        for i in range(n):
            r = np.random.rand()
            if r < p:
                c+=1

        return c

    def sigmoid(x):
        """
        Compute the sigmoid (logisitic) for x.
        :param x: The value to compute for.
        :return: The result.
        """
        return 1.0 / (1.0 + np.exp(-x))

    def output(self, input, w, b):
        """
        Calculate the sigmoid output for this layer.
        :param input: The input values for this layer's visable.
        :param w: Thw weights for this layer.
        :param b: The bias value for this layer.
        :return: The hidden values for this layer, the output.
        """
        linear_output = 0.0

        # First calculate the linear output.  Similar to linear regression.
        for j in range(self.input_count):
            linear_output += w[j] * input[j]

        linear_output += b

        # Now return the signoid of the linear sum.
        return HiddenLayer.sigmoid(linear_output)

    def sample_h_given_v(self,v,h):
        """
        Sample the hidden (h) output values, given the (v) input values.  This is different than the output method
        in that we are actually sampling discrete (0 or 1) values.
        :param v: The visible units.
        :param h: The hidden units, the count of how many times a true (1) was sampled.
        """
        for i in range(self.output_count):
            h[i] = self.binomial(1, self.output(v, self.weights[i], self.bias[i]))


class RestrictedBoltzmannMachine:
    def __init__(self, layer):
        self.layer = layer
        self.owner = layer.owner
        self.h_bias = layer.bias
        self.v_bias = np.zeros([self.get_visible_count()])

    def get_visible_count(self):
        return self.layer.input_count

    def get_hidden_count(self):
        return self.layer.output_count

class DeepBeliefNetwork:
    def __init__(self,input_count, hidden, output_count):
        self.layers = []
        self.rbm = []

        self.input_count = input_count
        self.input_count = output_count

        for i in range(len(hidden)):
            if i == 0:
                input_size = input_count
            else:
                input_size = hidden[i - 1]

            self.layers[i] = HiddenLayer(self,input_size, hidden[i])

            self.rbm[i] = RestrictedBoltzmannMachine(self.layers[i])

        self.output_layer = DeepLayer(self,hidden[len(hidden) - 1], output_count)


    def reset(self):
        for layer in self.layers:

            a = 1.0 / layer.input_count

            for j in range(layer.output_count):
                for k in range(layer.input_count):
                    layer.weights[j][k] = np.random.uniform(-a, a)

    def sigmoid(x):
        return 1.0 / (1.0 + np.exp(-x))

    def compute_regression(self, input):
        """
        Classify the input data into the list of probabilities of each class.
        :param input: The input.
        :return: An array that contains the probabilities of each class.
        """

        result = [0.0] * self.output_count
        layer_input = []
        prev_layer_input = input[:]

        for i in range(len(self.layers)):
            layer_input = [0.0] * self.layers[i].output_count

            for k in range(self.layers[i].output_count):
                output = 0.0

                for j in range(self.layers[i].input_count):
                    output += self.layers[i].weights[k][j] * prev_layer_input[j]

                output += self.layers[i].bias[k]
                layer_input[k] = self.sigmoid(output)

            if i < len(self.layers) - 1:
                prev_layer_input = layer_input[:]

        for i in range(self.output_layer.output_count):
            result[i] = 0
            for j in range(self.output_layer.input_count):
                result[i] += self.output_layer.weights[i][j] * layer_input[j]

            result[i] += self.output_layer.bias[i]

        self.output_layer.softmax(result)
        return result


class UnsupervisedTrainDBN:
    pass