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
    def __init__(self, inputCount, rbfCount, outputCount):
        self.inputCount = inputCount
        self.outputCount = outputCount

        # calculate input and output weight counts
        # add 1 to output to account for an extra bias node
        inputWeightCount = inputCount * rbfCount
        outputWeightCount = (rbfCount + 1) * outputCount
        rbfParams = (inputCount + 1) * rbfCount
        self.longTermMemory = np.zeros((inputWeightCount + outputWeightCount + rbfParams), dtype=float)

        self.indexInputWeights = 0
        self.indexOutputWeights = inputWeightCount + rbfParams

        self.rbf = {}

        # default the Rbf's to gaussian
        for i in xrange(0, rbfCount):
            rbfIndex = inputWeightCount + ((inputCount + 1) * i)
            self.rbf[i] = RbfGaussian(inputCount, self.longTermMemory, rbfIndex)


    def compute_regression(self, input):
        # first, compute the output values of each of the RBFs
        # Add in one additional RBF output for bias (always set to one).
        rbfOutput = [0] * (len(self.rbf) + 1)
        rbfOutput[len(rbfOutput) - 1] = 1.0; # bias

        for rbfIndex in xrange(0, len(self.rbf)):
            # weight the input
            weightedInput = [0] * len(input)

            for inputIndex in xrange(0, len(input)):
                memoryIndex = self.indexInputWeights + (rbfIndex * self.inputCount) + inputIndex
                weightedInput[inputIndex] = input[inputIndex] * self.longTermMemory[memoryIndex];

            # calculate the rbf
            rbfOutput[rbfIndex] = self.rbf[rbfIndex].evaluate(weightedInput)

        # second, calculate the output, which is the result of the weighted result of the RBF's.
        result = [0] * self.outputCount

        for outputIndex in xrange(0, len(result)):
            sum = 0
            for rbfIndex in xrange(0, len(rbfOutput)):
                # add 1 to rbf length for bias
                memoryIndex = self.indexOutputWeights + (outputIndex * (len(self.rbf) + 1)) + rbfIndex
                sum += rbfOutput[rbfIndex] * self.longTermMemory[memoryIndex]
            result[outputIndex] = sum;

        # finally, return the result.
        return result

    def reset(self):
        for i in xrange(0, len(self.longTermMemory)):
            self.longTermMemory[i] = np.random.uniform(0, 1)

    def compure_classification(self, input):
        output = self.compute_regression(input)
        return output.index(max(output))

    def copy_memory(self, source):
        for i in xrange(0, len(source)):
            self.longTermMemory[i] = source[i]