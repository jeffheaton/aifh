"""
    Artificial Intelligence for Humans
    Volume 1: Fundamental Algorithms
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com

    Code repository:
    https://github.com/jeffheaton/aifh

    Copyright 2013 by Jeff Heaton

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
__author__ = 'jheaton'

import numpy as np
from rbf import RbfGaussian


class RbfNetwork(object):
    """ A RBF network is an advanced machine learning algorithm that uses a series of RBF functions to perform
        regression.  It can also perform classification by means of one-of-n encoding.
        The long term memory of a RBF network is made up of the widths and centers of the RBF functions, as well as
        input and output weighting.
        http://en.wikipedia.org/wiki/RBF_network
    """
    def __init__(self, input_count, rbf_count, output_count):
        """ Create an RBF network with the specified shape.
        @param input_count: The input count.
        @param rbf_count: The RBF function count.
        @param output_count:  The output count.
        """
        self.input_count = input_count
        self.output_count = output_count

        # calculate input and output weight counts
        # add 1 to output to account for an extra bias node
        input_weight_count = input_count * rbf_count
        output_weight_count = (rbf_count + 1) * output_count
        rbf_params = (input_count + 1) * rbf_count
        self.long_term_memory = np.zeros((input_weight_count + output_weight_count + rbf_params), dtype=float)

        self.index_input_weights = 0
        self.index_output_weights = input_weight_count + rbf_params

        self.rbf = {}

        # default the Rbf's to gaussian
        for i in range(0, rbf_count):
            rbf_index = input_weight_count + ((input_count + 1) * i)
            self.rbf[i] = RbfGaussian(input_count, self.long_term_memory, rbf_index)


    def compute_regression(self, input):
        """ Compute the output for the network.
        @param input: The input pattern.
        @return: The output pattern.
        """
        # first, compute the output values of each of the RBFs
        # Add in one additional RBF output for bias (always set to one).
        rbf_output = [0] * (len(self.rbf) + 1)
        # bias
        rbf_output[len(rbf_output) - 1] = 1.0

        for rbfIndex in range(0, len(self.rbf)):
            # weight the input
            weighted_input = [0] * len(input)

            for inputIndex in range(0, len(input)):
                memory_index = self.index_input_weights + (rbfIndex * self.input_count) + inputIndex
                weighted_input[inputIndex] = input[inputIndex] * self.long_term_memory[memory_index]

            # calculate the rbf
            rbf_output[rbfIndex] = self.rbf[rbfIndex].evaluate(weighted_input)

        # Second, calculate the output, which is the result of the weighted result of the RBF's.
        result = [0] * self.output_count

        for outputIndex in range(0, len(result)):
            sum_value = 0
            for rbfIndex in range(0, len(rbf_output)):
                # add 1 to rbf length for bias
                memory_index = self.index_output_weights + (outputIndex * (len(self.rbf) + 1)) + rbfIndex
                sum_value += rbf_output[rbfIndex] * self.long_term_memory[memory_index]
            result[outputIndex] = sum_value

        # finally, return the result.
        return result

    def reset(self):
        """
        Reset the network to a random state.
        """
        for i in range(0, len(self.long_term_memory)):
            self.long_term_memory[i] = np.random.uniform(0, 1)

    def compute_classification(self, input):
        """ Compute the output and return the index of the output with the largest value.  This is the class that
        the network recognized.
        @param input: The input pattern.
        @return:
        """
        output = self.compute_regression(input)
        return output.index(max(output))

    def copy_memory(self, source):
        """ Copy the specified vector into the long term memory of the network.
        @param source: The source vector.
        """
        for i in range(0, len(source)):
            self.long_term_memory[i] = source[i]