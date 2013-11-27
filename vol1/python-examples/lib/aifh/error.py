__author__ = 'jheaton'

import numpy as np

class ErrorCalculation:
    def __init__(self):
        self.global_error = 0
        self.count = 0

    @staticmethod
    def rms(actual, ideal):
        return np.sqrt(np.mean((actual[:,:]-ideal[:,:])**2))

    @staticmethod
    def sse(actual, ideal):
        return np.sum((actual[:,:]-ideal[:,:])**2)

    @staticmethod
    def mse(actual, ideal):
        return np.mean((actual[:,:]-ideal[:,:])**2)