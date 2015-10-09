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
from energetic import EnergeticNetwork

class HopfieldNetwork(EnergeticNetwork):
    def __init__(self, neuron_count):
        super(HopfieldNetwork, self).__init__(neuron_count)
        self.input_count = neuron_count
        self.output_count = neuron_count
        self.activation_function = lambda d: 1 if (d > 0) else 0

    def compute(self, input):
        """
        Note: for Hopfield networks, you will usually want to call the "run"
        method to compute the output.

        This method can be used to copy the input data to the current state. A
        single iteration is then run, and the new current state is returned.
        :param input: The input pattern.
        :return: The new current state.
        """
        result = self.current_state[:]
        self.run()

        for i in range(self.current_state):
            result[i] = self.activation_function(self.current_state[i])

        self.current_state[:] = result
        return result

    def run(self):
        """
        Perform one Hopfield iteration.
        """
        for to_neuron in range(self.neuron_count):
            sum = 0
            for from_neuron in range(self.neuron_count):
                sum += self.current_state[from_neuron] \
                    * self.get_weight(from_neuron, to_neuron)
                self.current_state[to_neuron] = self.activation_function(sum)

    def run_until_stable(self, max_cycle):
        """
        Run the network until it becomes stable and does not change from more runs.
        :param max_cycle: The maximum number of cycles to run before giving up.
        :return: The number of cycles that were run.
        """

        done = False
        last_state_str = str(self.current_state)
        current_state_str = last_state_str

        cycle = 0
        while not done:
            self.run()
            cycle += 1

            last_state_str = str(self.current_state)

            if last_state_str == current_state_str:
                if cycle > max_cycle:
                    done = True
            else:
                done = True

            current_state_str = last_state_str

        return cycle

    def energy(self):
        t = 0

        # Calculate first term
        a = 0
        for i in range(self.input_count):
            for j in range(self.output_count):
                a += self.get_weight(i, j) * self.current_state[i] * self.current_state[j]
        a *= -0.5

        # Calculate second term
        b = 0
        for i in range(self.input_count):
            b += self.current_state[i] * t
        return a+b

class TrainHopfieldHebbian:
    def __init__(self, network):
        self.network = network;
        self.sum_matrix = np.zeros([network.input_count, network.input_count])
        self.pattern_count = 1

    def add_pattern(self, pattern):
        for i in range(self.network.input_count):
            for j in range(self.network.input_count):
                if i == j:
                    self.sum_matrix[i][j] = 0
                else:
                    self.sum_matrix[i][j] += pattern[i] * pattern[j]

        self.pattern_count += 1

    def learn(self):
        if self.pattern_count == 0:
            raise Exception("Please add a pattern before learning.  Nothing to learn.")

        for i in range(self.network.input_count):
            for j in range(self.network.input_count):
                self.network.set_weight(i, j, self.sum_matrix[i][j]/self.pattern_count)

class TrainHopfieldStorkey:
    def __init__(self, network):
        self.network = network;
        self.sum_matrix = np.zeros([network.input_count, network.input_count])

    def learn(self):
        """
        Learning is performed as patterns are added.
        """
        pass

    def calculate_local_field(self, i, pattern):
        result = 0
        for k in range(self.network.input_count):
            if k != i:
                result += self.network.get_weight(i, k) * pattern[k]
        return result

    def add_pattern(self, pattern):
        for i in range(self.network.input_count):
            for j in range(self.network.input_count):
                self.sum_matrix[i][j] = 0

        n = self.network.input_count
        for i in range(self.network.input_count):
            for j in range(self.network.input_count):
                t1 = (pattern[i] * pattern[j])/n
                t2 = (pattern[i] * self.calculate_local_field(j, pattern))/n
                t3 = (pattern[j] * self.calculate_local_field(i, pattern))/n
                d = t1-t2-t3
                self.sum_matrix[i][j] += d

        for i in range(self.network.input_count):
            for j in range(self.network.input_count):
                self.network.set_weight(i, j, self.network.get_weight(i, j)+self.sum_matrix[i][j])
