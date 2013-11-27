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

        for i in xrange(self.dimensions):
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

