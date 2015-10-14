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
        max = 0.0
        sum = 0.0

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
        self.visible_count = self.layer.input_count
        self.hidden_count = self.layer.output_count

    def binomial(self, n,p):
        if p < 0 or p > 1:
            return 0

        c = 0

        for i in range(n):
            r = np.random.rand()
            if r < p:
                c+=1
        return c


    def get_visible_count(self):
        return self.layer.input_count

    def get_hidden_count(self):
        return self.layer.output_count

class DeepBeliefNetwork:
    def __init__(self,input_count, hidden, output_count):
        self.layers = []
        self.rbm = []

        self.input_count = input_count
        self.output_count = output_count

        for i in range(len(hidden)):
            if i == 0:
                input_size = input_count
            else:
                input_size = hidden[i - 1]

            self.layers.append(HiddenLayer(self,input_size, hidden[i]))
            self.rbm.append(RestrictedBoltzmannMachine(self.layers[i]))

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
                layer_input[k] = DeepBeliefNetwork.sigmoid(output)

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
    """
    Unsupervised training for DBNN's. This class trains a single layer at a time.
    """
    def __init__(self,network, level, training_input, learning_rate, k):
        """
        Construct a trainer for upsupervised training for the DBNN.
        :param network: The DBNN to train.
        :param level: The level of the DBNN being trained.
        :param training_input: The training input cases.
        :param learning_rate: The learning rate.
        :param k: The number of cycles to use per iteration.
        """
        self.network = network
        self.level = level
        self.training_input = training_input
        self.learning_rate = learning_rate
        self.k = k

    def iteration(self):
        """
        Perform one iteration of unsupervised training on the DBN.
        """
        layer_input = None

        for row in self.training_input:

            # Perform layer-wise sample up to the layer being trained.
            for l in range(self.level+1):

                if l == 0:
                    layer_input = row[:]
                else:
                    # Get the previous input size, if we are on the first layer, this is the input count.
                    # Otherwise it is the input (visible) count from the previous layer.
                    if l == 1:
                        prev_layer_input_size = self.network.input_count
                    else:
                        prev_layer_input_size = self.network.layers[l - 1].input_count

                    # Copy the previous layer's input to a new array.
                    prev_layer_input = layer_input[:]

                    # Construct an array to hold the current layer's input
                    layer_input = [0.0] * self.network.layers[l].input_count

                    # Sample this layer's hidden neuron values (h), given previous layer's input (v).
                    # The output goes into layerInput, which is used in the next layer.
                    self.network.layers[l - 1].sample_h_given_v(prev_layer_input, layer_input)

            # Perform up-down algorithm.
            self.contrastive_divergence(self.network.rbm[self.level], layer_input, self.learning_rate, self.k)

    def contrastive_divergence(self, rbm, input, lr, k):
        """
        Perform contrastive divergence, also known as the up-down algorithm.
        :param rbm: The RBM to use.
        :param input: The input training pattern.
        :param lr: The learning rate.
        :param k: The number of cycles.
        """
        # The positive gradient mean & samples (P) - Only for hidden (H)
        mean_ph = [0.0] * rbm.hidden_count
        sample_ph = [0.0] * rbm.hidden_count
        # The negative gradient mean & samples (N) - For both visible (V) & hidden (H)
        means_nv = [0.0] * rbm.visible_count
        samples_nv = [0.0] * rbm.visible_count
        means_nh = [0.0] * rbm.hidden_count
        samples_nh = [0.0] * rbm.hidden_count

        # Calculate (sample) meanPH and samplePH
        self.sample_hv(rbm, input, mean_ph, sample_ph)

        for step in range(self.k):
            if step == 0:
                self.gibbs_hvh(rbm, sample_ph, means_nv, samples_nv, means_nh, samples_nh);
            else:
                self.gibbs_hvh(rbm, samples_nh, means_nv, samples_nv, means_nh, samples_nh)

        # Adjust the weights, based on calculated mean values.
        # This uses the maximum likelihood learning rule.
        for i in range(rbm.hidden_count):
            for j in range(rbm.visible_count):
                rbm.layer.weights[i][j] += lr *(mean_ph[i] * input[j] - means_nh[i] * samples_nv[j]) / len(input)
            rbm.h_bias[i] += lr * (sample_ph[i] - means_nh[i]) / len(input)

        # Adjust the biases for learning.
        for i in range(rbm.visible_count):
            rbm.v_bias[i] += lr * (input[i] - samples_nv[i]) / len(input)

    def sample_hv(self, rbm, v0sample, mean, sample):
        """
        Sample the hidden neurons (output), given the visible (input).  Return the mean, and a sample, based on that
        mean probability.
        :param rbm The RBM to use.
        :param v0Sample The input to the layer.
        :param mean Output: mean value of each hidden neuron.
        :param sample Output: sample, based on mean.
        """
        for i in range(rbm.hidden_count):
            # Find the mean.
            mean[i] = self.prop_up(rbm, v0sample, rbm.layer.weights[i], rbm.h_bias[i])
            # Sample, based on that mean.
            sample[i] = rbm.binomial(1, mean[i])

    def prop_up(self, rbm, v, w, b):
        """
        Estimate the mean of a hidden neuron in an RBM. Propagate upward part, from visible to hidden.
        :param rbm: The RBM to use.
        :param v: The input (v), visible neurons.
        :param w: The weights.
        :param b: The bias.
        :return: The mean.
        """
        sum = 0.0
        for j in range(rbm.visible_count):
            sum += w[j] * v[j]

        sum += b
        return DeepBeliefNetwork.sigmoid(sum)

    def gibbs_hvh(self, rbm, sample_h0, means_nv, samples_nv, means_nh, samples_nh):
        """
        Perform Gibbs sampling.  Hidden to visible to hidden.
        :param rbm: The RBM to use.
        :param sample_h0: The hidden samples.
        :param means_nv: Output: means for the visible (v) neurons.
        :param samples_nv: Output: samples for the visible (v) neurons.
        :param means_nh: Output: means for the hidden (h) neurons.
        :param samples_nh: Output: samples for the hidden (h) neurons.
        """
        self.sample_vh(rbm, sample_h0, means_nv, samples_nv)
        self.sample_hv(rbm, samples_nv, means_nh, samples_nh)

    def sample_vh(self, rbm, sample_h0, mean, sample):
        """
        Sample the visible (input), given the hidden neurons (output).  Return the mean, and a sample, based on that
        mean probability.
        :param rbm: The RBM to use.
        :param sample_h0: Hidden (h) samples.
        :param mean: Output: Visible (v) mean.
        :param sample: Output: Visible (v) sample.
        """
        for i in range(rbm.visible_count):
            mean[i] = self.prop_down(rbm, sample_h0, i, rbm.v_bias[i])
            sample[i] = rbm.binomial(1, mean[i])

    def prop_down(self, rbm, h, i, b):
        """
        Estimate the mean of a visible neuron in an RBM. Propagate downward part, from hidden to visible.
        :param rbm: The RBM to use.
        :param h: The hidden neurons.
        :param i: The visible neuron to use.
        :param b: Bias value.
        :return: The estimated mean
        """
        sum = 0.0
        for j in range(rbm.hidden_count):
            sum += rbm.layer.weights[j][i] * h[j]

        sum += b
        return DeepBeliefNetwork.sigmoid(sum)

class SupervisedTrainDBN:
    def __init__(self, network, training_input, training_ideal, learning_rate):
        self.network = network
        self.training_input = training_input
        self.learning_rate = learning_rate
        self.training_ideal = training_ideal
        self.total_error = 0.0


    def iteration(self):
        self.total_error = 0
        for n in range(len(self.training_input)):
            row = self.training_input[n]
            for i in range(len(self.network.layers)):
                if i == 0:
                    prev_layer_input = row[:]
                else:
                    prev_layer_input = layer_input[:]


                layer_input = [0.0] * self.network.layers[i].output_count
                self.network.layers[i].sample_h_given_v(prev_layer_input, layer_input)


            self.train_logistic_layer(layer_input, self.training_ideal[n])


    def train_logistic_layer(self, input, ideal):
        p_y_given_x = [0.0] * self.network.output_layer.output_count
        dy = [0.0] * self.network.output_layer.output_count

        for i in range(self.network.output_layer.output_count):
            p_y_given_x[i] = 0
            for j in range(self.network.output_layer.input_count):
                p_y_given_x[i] += self.network.output_layer.weights[i][j] * input[j]

            p_y_given_x[i] += self.network.output_layer.bias[i]

        self.network.output_layer.softmax(p_y_given_x)

        for i in range(self.network.output_layer.output_count):
            dy[i] = ideal[i] - p_y_given_x[i]
            self.total_error+=np.power(dy[i],2)

            for j in range(self.network.output_layer.input_count):
                self.network.output_layer.weights[i][j] += self.learning_rate * dy[i] * input[j] / len(self.training_input)

            self.network.output_layer.bias[i] += self.learning_rate * dy[i] / len(self.training_input)

    def error(self):
        """
        :return: The SSE error.
        """
        return self.total_error / len(self.training_input)