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
from energetic import EnergeticNetwork
import numpy as np
import math

class BoltzmannMachine(EnergeticNetwork):
    def __init__(self, neuron_count):
        super(BoltzmannMachine, self).__init__(neuron_count)

        # The current temperature of the neural network. The higher the
        # temperature, the more random the network will behave.
        self.temperature = 0

        # The thresholds.
        self.threshold = [0] * neuron_count

        # Count used to internally determine if a neuron is "on".
        self.on = [0] * neuron_count

        # Count used to internally determine if a neuron is "off".
        self.off = [0] * neuron_count

        # The number of cycles to anneal for.
        self.anneal_cycles = 100

        # The number of cycles to run the network through before annealing.
        self.run_cycles = 1000

    def compute(self, input):
        """
        Note: for Boltzmann networks, you will usually want to call the "run"
        method to compute the output.

        This method can be used to copy the input data to the current state. A
        single iteration is then run, and the new current state is returned.
        :param input: The input pattern.
        :return: The new current state.
        """
        result = self.current_state[:]
        self.run()
        self.current_state[:] = result[:]
        return result

    def decrease_temperature(self, d):
        """
        Decrease/increase the temperature by the specified amount.
        :param d: The amount to decrease by, for example .8 to change to
        80% of current.
        """
        self.temperature *= d

    def establish_equilibrium(self):
        count = self.neuron_count

        for i in range(0, count):
            self.on[i] = 0
            self.off[i] = 0

        for n in range(0, self.run_cycles * count):
            self.run(np.random.randint(0, count - 1))

        for n in range(0, self.anneal_cycles * count):
            i = np.random.randint(0, count - 1)
            self.run(i)
            if self.current_state[i]>0:
                self.on[i] += 1
            else:
                self.off[i] += 1

        for i in range(0,count):
            self.current_state[i] = 1 if self.on[i] > self.off[i] else 0

    def run(self):
        """
        Run the network for all neurons present.
        """
        count = self.neuron_count
        for i in range(0, count):
            self.run(i)

    def run(self, i):
        """
         Run the network for the specified neuron.
        :param i: The neuron to run for.
        """
        j = 0
        sum = 0
        probability = 0
        count = self.neuron_count

        sum = 0
        for j in range(0, count):
            sum += self.get_weight(i, j) * (1 if (self.current_state[j] > 0) else 0)

        sum -= self.threshold[i]
        probability = 1 / (1 + math.exp(-sum / self.temperature))
        if np.random.rand() <= probability:
            self.current_state[i] = 1.0
        else:
            self.current_state[i] = 0.0
