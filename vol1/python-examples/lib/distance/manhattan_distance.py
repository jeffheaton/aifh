#!/usr/bin/env python
# Programmer: Chris Bunch (shatterednirvana@gmail.com)


import math


from abstract_distance import AbstractDistance


class ManhattanDistance(AbstractDistance):


  def calculate_from_positions(self, position1, pos1, position2, pos2, length):
    result = 0.0
    for index in range(length):
      diff = math.fabs(position1[index] - position2[index])
      result += diff
    return result
