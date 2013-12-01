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