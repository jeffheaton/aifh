__author__ = 'jheaton'

import os
import sys

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)


from euclidean_distance import EuclideanDistance
from manhattan_distance import ManhattanDistance
from chebyshev_distance import ChebyshevDistance


pos1 = [ 1.0, 2.0, 3.0 ]
pos2 = [ 4.0, 5.0, 6.0 ]
pos3 = [ 7.0, 8.0, 9.0 ]

calcE = EuclideanDistance()
calcM = ManhattanDistance()
calcC = ChebyshevDistance()

print("Euclidean Distance")
print("pos1->pos2: ", calcE.calculate(pos1,pos2))
print("pos2->pos3: ", calcE.calculate(pos2,pos3))
print("pos3->pos1: ", calcE.calculate(pos3,pos1))
print("\nManhattan Distance\n")
print("pos1->pos2: ", calcM.calculate(pos1,pos2))
print("pos2->pos3: ", calcM.calculate(pos2,pos3))
print("pos3->pos1: ", calcM.calculate(pos3,pos1))
print("\nChebyshev Distance\n")
print("pos1->pos2: ", calcC.calculate(pos1,pos2))
print("pos2->pos3: ", calcC.calculate(pos2,pos3))
print("pos3->pos1: ", calcC.calculate(pos3,pos1))
