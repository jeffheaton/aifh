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

class NeighborhoodRBF:
    """
    Implements a multi-dimensional RBF neighborhood function.
    """
    TYPE_GAUSSIAN = 0
    TYPE_MULTIQUADRIC = 1
    TYPE_INVERSE_MULTIQUADRIC = 2
    TYPE_MEXICAN_HAT = 3

    SQ75 = np.sqrt(0.75)

    def __init__(self, type, size):
        """
        Construct a 2d neighborhood function based on the sizes for the
        x and y dimensions.
        :param type: The RBF type to use.
        :param x: The size of the x-dimension.
        :param y: The size of the y-dimension.
        """
        # The radial basis function to use.
        self.rbf = None

        # The size of each dimension.
        self.size = size[:]

        # The displacement of each dimension, when mapping the dimensions
        # to a 1d array.
        self.displacement = []

        self.hexagon = False

        self.type = type

        self.width = 1

        self.calculate_displacement()

    def calculate_displacement(self):
        """
        Calculate all of the displacement values.
        """
        self.displacement = np.zeros(len(self.size))
        for i in range(len(self.size)):
            if i == 0:
                value = 0
            elif i == 1:
                value = self.size[0]
            else:
                value = self.displacement[i - 1] * self.size[i - 1]

            self.displacement[i] = int(value)

    def translate_coordinates(self, index):
        """
        Translate the specified index into a set of multi-dimensional
        coordinates that represent the same index.  This is how the
        multi-dimensional coordinates are translated into a one dimensional
        index for the input neurons.
        :param index: The index to translate.
        :return: The multi-dimensional coordinates.
        """
        result = [0] * len(self.displacement)
        counting_index = int(index)

        for i in range(len(self.displacement)-1,-1,-1):
            if self.displacement[i] > 0:
                value = counting_index // self.displacement[i]
            else:
                value = counting_index

            counting_index -= self.displacement[i] * value
            result[i] = value

        #print("{} -> {}".format(index, result))

        return result

    def fn(self, current_neuron, best_neuron):
        """
        Calculate the value for the multi RBF function.
        :param current_neuron: The current neuron.
        :param best_neuron: The best neuron.
        :return: A percent that determines the amount of training the current
        neuron should get.  Usually 100% when it is the bestNeuron.
        """
        vector = np.zeros(len(self.displacement))
        vector_current = self.translate_coordinates(current_neuron)
        vector_best = self.translate_coordinates(best_neuron)
        for i in range(len(vector_current)):
            vector[i] = vector_current[i] - vector_best[i]

        if self.hexagon:

            if len(self.size) !=2:
                raise Exception("Hexagon lattice can only be used in two dimensions.")

            row = vector[1]
            col = vector[0]
            even_indent = 1
            odd_indent = 2.5
            indent = odd_indent if row%2==1 else even_indent

            vector[1] = int(NeighborhoodRBF.SQ75+(row * NeighborhoodRBF.SQ75))
            vector[0] = int(indent+(3*col))

        if self.type == NeighborhoodRBF.TYPE_GAUSSIAN:
            value = 0

            for i in range(len(self.size)):
                value += np.power(vector[i], 2) / (2.0 * self.width * self.width)
            return np.exp(-value)

        elif self.type == NeighborhoodRBF.TYPE_MULTIQUADRIC:
            value = 0

            for i in range(len(self.size)):
                value += np.pow(vector[i], 2) + (self.width * self.width)
            return np.sqrt(value)
        elif self.type == NeighborhoodRBF.TYPE_INVERSE_MULTIQUADRIC:
            value = 0
            for i in range(len(self.size)):
                value += np.pow(vector[i], 2) + (self.width * self.width)
            return 1 / np.sqrt(value)
        elif self.type == NeighborhoodRBF.TYPE_MEXICAN_HAT:
            # calculate the "norm", but don't take square root
            # don't square because we are just going to square it
            norm = 0
            for i in range(len(self.size)):
                norm += np.pow(vector[i], 2)

            # calculate the value
            return (1 - norm) * np.exp(-norm / 2)
        else:
            raise Exception("Invalid RBF function type: {}".format(self.type))
