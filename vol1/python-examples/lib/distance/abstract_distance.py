#!/usr/bin/env python
# Programmer: Chris Bunch (shatterednirvana@gmail.com)


from calculate_distance import CalculateDistance


class AbstractDistance(CalculateDistance):


  def calculate(self, position1, position2):
    return self.calculate_from_positions(position1, 0, position2, 0, len(position1))
