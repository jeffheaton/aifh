__author__ = 'jheaton'

import math
import numpy as np
from scipy.spatial import distance


class Equilateral(object):
    def __init__(self, class_count, normalized_low, normalized_high):
        """ Create a lookup table that will be used for both Equilateral encoding and decoding.
        """
        # Allocate a matrix to hold the lookup table.

        self.encoded = np.ndarray(shape=(class_count, class_count - 1), dtype=float)

        # Seed the result.
        self.encoded[0][0] = -1
        self.encoded[1][0] = 1.0

        for k in range(2, class_count):
            # scale the matrix so far
            r = k
            f = math.sqrt(r * r - 1.0) / r
            for i in range(0, k):
                for j in range(0, k - 1):
                    self.encoded[i][j] *= f

            r = -1.0 / r
            for i in range(0, k):
                self.encoded[i][k - 1] = r

            for i in range(0, k - 1):
                self.encoded[k][i] = 0.0

            self.encoded[k][k - 1] = 1.0

        # Scale it.
        min_eq = -1
        max_eq = 1
        for row in range(0, len(self.encoded)):
            for col in range(0, len(self.encoded[row])):
                self.encoded[row][col] = ((self.encoded[row][col] - min_eq) / (max_eq - min_eq)) \
                    * (normalized_high - normalized_low) + normalized_low

    def encode(self, class_num):
        """ Provide the equilateral encoded vector for the specified class.
        """
        return self.encoded[class_num]

    def decode(self, vec):
        """ Match the specified vector to the class that it most closely fits.
        """
        min_dist = float('inf')
        result = -1

        for i in range(0, len(self.encoded)):
            dist = distance.euclidean(vec, self.encoded[i])
            if dist < min_dist:
                result = i
                min_dist = dist

        return result