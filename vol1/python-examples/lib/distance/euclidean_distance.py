#!/usr/bin/env python
# Programmer: Chris Bunch (shatterednirvana@gmail.com)


import math


from abstract_distance import AbstractDistance


class EuclideanDistance(AbstractDistance):


  def calculate_from_positions(self, position1, pos1, position2, pos2, length):
    squared_sum = 0.0
    for index in range(length):
      diff = position1[pos1 + index] - position2[pos2 + index]
      squared_sum += diff * diff
    return math.sqrt(squared_sum)
