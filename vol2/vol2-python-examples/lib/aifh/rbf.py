"""
    Artificial Intelligence for Humans
    Volume 2: Nature-Inspired Algorithms
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com

    Code repository:
    https://github.com/jeffheaton/aifh

    Copyright 2014 by Jeff Heaton

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


class RbfFunction(object):
    def __init__(self, dimensions, params, index):
        self.dimensions = dimensions
        self.params = params
        self.index = index

    @property
    def width(self):
        return self.params[self.index]

    @width.setter
    def width(self, value):
        self.params[self.index] = value

    def set_center(self, index, value):
        self.params[self.index + index + 1] = value

    def get_center(self, index):
        return self.params[self.index + index + 1]


class RbfGaussian(RbfFunction):
    def evaluate(self, x):
        value = 0
        width = self.width

        for i in range(self.dimensions):
            center = self.get_center(i)
            value += ((x[i] - center) ** 2) / (2.0 * width * width)
        return np.exp(-value)


class RbfMexicanHat(RbfFunction):
    def evaluate(self, x):
        # Calculate the "norm", but don't take square root.
        # Don't square because we are just going to square it.
        norm = 0
        for i in xrange(self.dimensions):
            center = self.get_center(i)
            norm += (x[i] - center) ** 2

        # Calculate the value.
        return (1 - norm) * np.exp(-norm / 2)


class RbfMultiquadric(RbfFunction):
    def evaluate(self, x):
        value = 0
        width = self.width

        for i in xrange(self.dimensions):
            center = self.get_center(i)
            value += (x[i] - center) ** 2 + (width * width)

        return np.sqrt(value)


class RbfInverseMultiquadric(RbfFunction):
    def evaluate(self, x):
        value = 0
        width = self.width

        for i in xrange(self.dimensions):
            center = self.get_center(i)
            value += (x[i] - center) ** 2 + (width * width)

        return 1.0 / np.sqrt(value)

