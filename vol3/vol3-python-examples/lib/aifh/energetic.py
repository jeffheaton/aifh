__author__ = 'jeff'

import numpy as np

class EnergeticNetwork:
    """
    The energetic network forms the base class for Hopfield and Boltzmann machines.
    """

    def __init__(self, neuron_count):
        """
        Construct the network with the specified neuron count.
        :param neuron_count: The number of neurons.
        """
        # The current state of the thermal network.
        self.current_state = [0.0] * neuron_count

        # The weights.
        self.weights = np.zeros( [neuron_count*neuron_count] )

        # The neuron count.
        self.neuron_count = neuron_count

    def add_weight(self, from_neuron, to_neuron, value):
        """
        Add to the specified weight.
        :param from_neuron: The from neuron.
        :param to_neuron: The to neuron.
        :param value: The value to add.
        """
        index = (to_neuron * self.neuron_count) + from_neuron
        if index >= len(self.weights):
            raise IndexError("Out of range: from_neuron: {}, to_neuron: {}".format(from_neuron, to_neuron))
        self.weights[index] += value

    def calculate_energy(self):
        """
        Calculate the energy for the network.
        :return:Calculate the current energy for the network. The network will
        seek to lower this value.
        """
        temp_e = 0

        for i in range(0,self.neuron_count):
            for j in range(0, self.neuron_count):
                if i != j:
                    temp_e += self.get_weight(i, j) * self.current_state[i] * \
                        self.current_state[j]
        return -1 * temp_e / 2

    def clear(self):
        """
        Clear any connection weights.
        """
        for i in range(0, len(self.weights)):
            self.weights[i] = 0

    def get_weight(self, from_neuron, to_neuron):
        """
        Get a weight.
        :param from_neuron: The from neuron.
        :param to_neuron: The to neuron.
        :return: The weight.
        """
        index = (to_neuron * self.neuron_count) + from_neuron
        return self.weights[index]

    def init(self, neuron_count, weights, output):
        """
        Init the network.
        :param neuron_count: The neuron count.
        :param weights: The weights.
        :param output: The output.
        """
        if neuron_count != len(output):
            raise IndexError("Neuron count({}) must match output count({}).".format(neuron_count, len(output)))

        if (neuron_count * neuron_count) != len(weights):
            raise(Exception("Weight count({}) must be the square of the neuron count({}).".format(len(weights.length))))

        self.neuron_count = neuron_count
        self.weights = weights
        self.current_state = [0] * neuron_count
        for i in range(0,len(self.current_state)):
            self.current_state[i] = output[i]

    def reset(self, rnd):
        """
        Reset the neural network to random weights, this is not used for Hopfield.
        :param rnd: Random number generator
        """
        for i in range(0, len(self.current_state)):
            self.current_state[i] = 0

        for i in range(0, len(self.weights)):
            self.weights[i] = 0

    def set_current_state(self, s):
        """
        Set the current state.
        :param s: The current state array.
        """
        self.current_state[:] = s[:]

    def set_weight(self, from_neuron, to_neuron, value):
        """
        Set a weight.
        :param from_neuron: The from neuron.
        :param to_neuron: The to neuron.
        :param value: The value to set.
        """
        index = (to_neuron * self.neuron_count) + from_neuron
        self.weights[index] = value
