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
import scipy.spatial
import numpy as np
import scipy as sp
import sys


class SelfOrganizingMap:
    """
    The weights of the output neurons base on the input from the input
    neurons.
    """

    def __init__(self, input_count, output_count):
        """
        The constructor.
        :param input_count: Number of input neurons
        :param output_count: Number of output neurons
        :return:
        """
        self.input_count = input_count
        self.output_count = output_count
        self.weights = np.zeros([self.output_count, self.input_count])

        self.distance = sp.spatial.distance.euclidean

    def calculate_error(self, data):
        bmu = BestMatchingUnit(self)

        bmu.reset()

        # Determine the BMU for each training element.
        for input in data:
            bmu.calculate_bmu(input)

        # update the error
        return bmu.worst_distance / 100.0

    def classify(self, input):
        if len(input) > self.input_count:
            raise Exception("Can't classify SOM with input size of {} "
                            "with input data of count {}".format(self.input_count, len(input)))

        min_dist = sys.maxfloat
        result = -1

        for i in range(self.output_count):
            dist = self.distance.calculate(input, self.weights[i])
            if dist < min_dist:
                min_dist = dist
                result = i

        return result

    def reset(self):
        self.weights = (np.random.rand(self.weights.shape[0], self.weights.shape[1]) * 2.0) - 1


class BestMatchingUnit:
    """
    The "Best Matching Unit" or BMU is a very important concept in the training
    for a SOM. The BMU is the output neuron that has weight connections to the
    input neurons that most closely match the current input vector. This neuron
    (and its "neighborhood") are the neurons that will receive training.

    This class also tracks the worst distance (of all BMU's). This gives some
    indication of how well the network is trained, and thus becomes the "error"
    of the entire network.
    """

    def __init__(self, som):
        """
        Construct a BestMatchingUnit class.  The training class must be provided.
        :param som: The SOM to evaluate.
        """
        # The owner of this class.
        self.som = som

        # What is the worst BMU distance so far, this becomes the error for the
        # entire SOM.
        self.worst_distance = 0

    def calculate_bmu(self, input):
        """
        Calculate the best matching unit (BMU). This is the output neuron that
        has the lowest Euclidean distance to the input vector.
        :param input: The input vector.
        :return: The output neuron number that is the BMU.
        """
        result = 0

        if len(input) > self.som.input_count:
            raise Exception(
                "Can't train SOM with input size of {}  with input data of count {}.".format(self.som.input_count,
                                                                                             len(input)))

        # Track the lowest distance so far.
        lowest_distance = float("inf")

        for i in range(self.som.output_count):
            distance = self.calculate_euclidean_distance(self.som.weights, input, i)

            # Track the lowest distance, this is the BMU.
            if distance < lowest_distance:
                lowest_distance = distance
                result = i

        # Track the worst distance, this is the error for the entire network.
        if lowest_distance > self.worst_distance:
            self.worst_distance = lowest_distance

        return result

    def calculate_euclidean_distance(self, matrix, input, output_neuron):
        """
        Calculate the Euclidean distance for the specified output neuron and the
        input vector.  This is the square root of the squares of the differences
        between the weight and input vectors.
        :param matrix: The matrix to get the weights from.
        :param input: The input vector.
        :param outputNeuron: The neuron we are calculating the distance for.
        :return: The Euclidean distance.
        """
        result = 0

        # Loop over all input data.
        diff = input - matrix[output_neuron]
        return np.sqrt(sum(diff*diff))


class BasicTrainSOM:
    """
    This class implements competitive training, which would be used in a
    winner-take-all neural network, such as the self organizing map (SOM). This
    is an unsupervised training method, no ideal data is needed on the training
    set. If ideal data is provided, it will be ignored.

    Training is done by looping over all of the training elements and calculating
    a "best matching unit" (BMU). This BMU output neuron is then adjusted to
    better "learn" this pattern. Additionally, this training may be applied to
    other "nearby" output neurons. The degree to which nearby neurons are update
    is defined by the neighborhood function.

    A neighborhood function is required to determine the degree to which
    neighboring neurons (to the winning neuron) are updated by each training
    iteration.

    Because this is unsupervised training, calculating an error to measure
    progress by is difficult. The error is defined to be the "worst", or longest,
    Euclidean distance of any of the BMU's. This value should be minimized, as
    learning progresses.

    Because only the BMU neuron and its close neighbors are updated, you can end
    up with some output neurons that learn nothing. By default these neurons are
    not forced to win patterns that are not represented well. This spreads out
    the workload among all output neurons. This feature is not used by default,
    but can be enabled by setting the "forceWinner" property.
    """

    def __init__(self, network, learning_rate, training, neighborhood):

        # The neighborhood function to use to determine to what degree a neuron
        # should be "trained".
        self.neighborhood = neighborhood

        # The learning rate. To what degree should changes be applied.
        self.learning_rate = learning_rate

        # The network being trained.
        self.network = network

        # How many neurons in the input layer.
        self.input_neuron_count = network.input_count

        # How many neurons in the output layer.
        self.output_neuron_count = network.output_count

        # Utility class used to determine the BMU.
        self.bmu_util = BestMatchingUnit(network)

        # Correction matrix.
        self.correction_matrix = np.zeros([network.output_count, network.input_count])

        # True is a winner is to be forced, see class description, or forceWinners
        # method. By default, this is true.
        self.force_winner = False

        # When used with autodecay, this is the starting learning rate.
        self.start_rate = 0

        # When used with autodecay, this is the ending learning rate.
        self.end_rate = 0

        # When used with autodecay, this is the starting radius.
        self.start_radius = 0

        # When used with autodecay, this is the ending radius.
        self.end_radius = 0

        # This is the current autodecay learning rate.
        self.auto_decay_rate = 0

        # This is the current autodecay radius.
        self.auto_decay_radius = 0

        # The current radius.
        self.radius = 0

        # Training data.
        self.training = training

    def _apply_correction(self):
        """
        Loop over the synapses to be trained and apply any corrections that were
        determined by this training iteration.
        """
        np.copyto(self.network.weights, self.correction_matrix)

    def auto_decay(self):
        """
        Should be called each iteration if autodecay is desired.
        """
        if self.radius > self.end_radius:
            self.radius += self.auto_decay_radius

        if self.learning_rate > self.end_rate:
            self.learning_rate += self.auto_decay_rate

        self.neighborhood.radius = self.radius

    def copy_input_pattern(self, matrix, output_neuron, input):
        """
        Copy the specified input pattern to the weight matrix. This causes an
        output neuron to learn this pattern "exactly". This is useful when a
        winner is to be forced.
        :param matrix: The matrix that is the target of the copy.
        :param output_neuron: The output neuron to set.
        :param input: The input pattern to copy.
        """
        matrix[output_neuron, :] = input

    def decay(self, decay_rate, decay_radius):
        """
        Decay the learning rate and radius by the specified amount.
        :param decay_rate: The percent to decay the learning rate by.
        :param decay_radius: The percent to decay the radius by.
        """
        self.radius *= (1.0 - decay_radius)
        self.learning_rate *= (1.0 - decay_rate)
        self.neighborhood.radius = self.radius

    def _determine_new_weight(self, weight, input, currentNeuron, bmu):
        """
        Determine the weight adjustment for a single neuron during a training
        iteration.
        :param weight: The starting weight.
        :param input: The input to this neuron.
        :param currentNeuron: The neuron who's weight is being updated.
        :param bmu: The neuron that "won", the best matching unit.
        :return: The new weight value.
        """
        return weight \
               + (self.neighborhood.fn(currentNeuron, bmu) \
                  * self.learning_rate * (input - weight))

    def _force_winners(self, matrix, won, least_represented):
        """
        Force any neurons that did not win to off-load patterns from overworked
        neurons.
        :param matrix: An array that specifies how many times each output neuron has "won".
        :param won: The training pattern that is the least represented by this neural network.
        :param least_represented: The synapse to modify.
        :return: True if a winner was forced.
        """
        max_activation = float("-inf")
        max_activation_neuron = -1

        output = self.compute(self.network, self.least_represented)

        # Loop over all of the output neurons. Consider any neurons that were
        # not the BMU (winner) for any pattern. Track which of these
        # non-winning neurons had the highest activation.
        for output_neuron in range(len(won)):
            # Only consider neurons that did not "win".
            if won[output_neuron] == 0:
                if (max_activation_neuron == -1) \
                        or (output[output_neuron] > max_activation):
                    max_activation = output[output_neuron]
                    max_activation_neuron = output_neuron

        # If a neurons was found that did not activate for any patterns, then
        # force it to "win" the least represented pattern.
        if max_activation_neuron != -1:
            self.copy_input_pattern(matrix, max_activation_neuron, least_represented)
            return True
        else:
            return False

    def iteration(self):
        """
        Perform one training iteration.
        """
        # Reset the BMU and begin this iteration.
        self.bmu_util.reset()
        won = [0] * self.output_neuron_count
        least_represented_activation = float("inf")
        least_represented = None

        # Reset the correction matrix for this synapse and iteration.
        self.correctionMatrix.clear()

        # Determine the BMU for each training element.
        for input in self.training:
            bmu = self.bmu_util.calculate_bmu(input)
            won[bmu] += 1

            # If we are to force a winner each time, then track how many
            # times each output neuron becomes the BMU (winner).
            if self.force_winner:
                # Get the "output" from the network for this pattern. This
                # gets the activation level of the BMU.
                output = self.compute(self.network, input)

                # Track which training entry produces the least BMU. This
                # pattern is the least represented by the network.
                if output[bmu] < least_represented_activation:
                    least_represented_activation = output[bmu]
                    least_represented = input.getInput()

            self.train(bmu, self.network.getWeights(), input.getInput())

            if self.force_winner:
                # force any non-winning neurons to share the burden somewhat
                if not self.force_winners(self.network.weights, won, least_represented):
                    self.apply_correction()
            else:
                self.apply_correction()

    def set_auto_decay(self, planned_iterations, start_rate, end_rate, start_radius, end_radius):
        """
        Setup autodecay. This will decrease the radius and learning rate from the
        start values to the end values.
        :param planned_iterations: The number of iterations that are planned. This allows the
        decay rate to be determined.
        :param start_rate: The starting learning rate.
        :param end_rate: The ending learning rate.
        :param start_radius: The starting radius.
        :param end_radius: The ending radius.
        """
        self.start_rate = start_rate
        self.end_rate = end_rate
        self.start_radius = start_radius
        self.end_radius = end_radius
        self.auto_decay_radius = (end_radius - start_radius) / planned_iterations
        self.auto_decay_rate = (end_rate - start_rate) / planned_iterations
        self.set_params(self.start_rate, self.start_radius)

    def set_params(self, rate, radius):
        """
        Set the learning rate and radius.
        :param rate: The new learning rate.
        :param radius:
        :return: The new radius.
        """
        self.radius = radius
        self.learning_rate = rate
        self.neighborhood.radius = radius

    def get_status(self):
        """
        :return: A string display of the status.
        """
        result = "Rate="
        result += str(self.learning_rate)
        result += ", Radius="
        result += str(self.radius)
        return result

    def _train(self, bmu, matrix, input):
        """
        Train for the specified synapse and BMU.
        :param bmu: The best matching unit for this input.
        :param matrix: The synapse to train.
        :param input: The input to train for.
        :return:
        """
        # adjust the weight for the BMU and its neighborhood
        for output_neuron in range(self.output_neuron_count):
            self._train_pattern(matrix, input, output_neuron, bmu)

    def _train_pattern(self, matrix, input, current, bmu):
        """
        Train for the specified pattern.
        :param matrix: The synapse to train.
        :param input: The input pattern to train for.
        :param current: The current output neuron being trained.
        :param bmu: The best matching unit, or winning output neuron.
        """

        for input_neuron in range(self.input_neuron_count):
            current_weight = matrix[current][input_neuron]
            input_value = input[input_neuron]

            new_weight = self._determine_new_weight(current_weight,
                    input_value, current, bmu)

            self.correction_matrix[current][input_neuron] = new_weight

    def train_single_pattern(self, pattern):
        """
        Train the specified pattern. Find a winning neuron and adjust all neurons
        according to the neighborhood function.
        :param pattern: The pattern to train.
        """
        bmu = self.bmu_util.calculate_bmu(pattern)
        self._train(bmu, self.network.weights, pattern)
        self._apply_correction()

    def compute(self, som, input):
        """
        Calculate the output of the SOM, for each output neuron.  Typically,
        you will use the classify method instead of calling this method.
        :param som: The input pattern.
        :param input: The output activation of each output neuron.
        :return:
        """

        result = np.zeros(som.output_count)

        for i in range(som.output_count):
            optr = som.weights[i]

            matrix_a = np.zeros([input.length,1])
            for j in range(len(input)):
                matrix_a[0][j] = input[j]

            matrix_b = np.zeros(1,input.length)
            for j in range(len(optr)):
                matrix_b[0][j] = optr[j]

            result[i] = np.dot(matrix_a, matrix_b)

        return result
