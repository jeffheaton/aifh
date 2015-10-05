#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 3: Deep Learning and Neural Networks
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com
    Code repository:
    https://github.com/jeffheaton/aifh
    Copyright 2015 by Jeff Heaton
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

import os
import sys

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from hopfield import *
import numpy as np

# The neural network will learn these patterns.
PATTERN =  [[
            "O O O O O ",
            " O O O O O",
            "O O O O O ",
            " O O O O O",
            "O O O O O ",
            " O O O O O",
            "O O O O O ",
            " O O O O O",
            "O O O O O ",
            " O O O O O"],

          [ "OO  OO  OO",
            "OO  OO  OO",
            "  OO  OO  ",
            "  OO  OO  ",
            "OO  OO  OO",
            "OO  OO  OO",
            "  OO  OO  ",
            "  OO  OO  ",
            "OO  OO  OO",
            "OO  OO  OO"  ],

          [ "OOOOO     ",
            "OOOOO     ",
            "OOOOO     ",
            "OOOOO     ",
            "OOOOO     ",
            "     OOOOO",
            "     OOOOO",
            "     OOOOO",
            "     OOOOO",
            "     OOOOO"  ],

          [ "O  O  O  O",
            " O  O  O  ",
            "  O  O  O ",
            "O  O  O  O",
            " O  O  O  ",
            "  O  O  O ",
            "O  O  O  O",
            " O  O  O  ",
            "  O  O  O ",
            "O  O  O  O"  ],

          [ "OOOOOOOOOO",
            "O        O",
            "O OOOOOO O",
            "O O    O O",
            "O O OO O O",
            "O O OO O O",
            "O O    O O",
            "O OOOOOO O",
            "O        O",
            "OOOOOOOOOO"  ]]

# The neural network will be tested on these patterns, to see
# which of the last set they are the closest to.
PATTERN2 = [[
            "          ",
            "          ",
            "          ",
            "          ",
            "          ",
            " O O O O O",
            "O O O O O ",
            " O O O O O",
            "O O O O O ",
            " O O O O O"],

           ["OOO O    O",
            " O  OOO OO",
            "  O O OO O",
            " OOO   O  ",
            "OO  O  OOO",
            " O OOO   O",
            "O OO  O  O",
            "   O OOO  ",
            "OO OOO  O ",
            " O  O  OOO"],

           ["OOOOO     ",
            "O   O OOO ",
            "O   O OOO ",
            "O   O OOO ",
            "OOOOO     ",
            "     OOOOO",
            " OOO O   O",
            " OOO O   O",
            " OOO O   O",
            "     OOOOO"],

           ["O  OOOO  O",
            "OO  OOOO  ",
            "OOO  OOOO ",
            "OOOO  OOOO",
            " OOOO  OOO",
            "  OOOO  OO",
            "O  OOOO  O",
            "OO  OOOO  ",
            "OOO  OOOO ",
            "OOOO  OOOO"],

           ["OOOOOOOOOO",
            "O        O",
            "O        O",
            "O        O",
            "O   OO   O",
            "O   OO   O",
            "O        O",
            "O        O",
            "O        O",
            "OOOOOOOOOO"]]

HEIGHT = 10
WIDTH = 10

def convert_pattern(data, index):
    result_index = 0
    result = np.zeros([WIDTH*HEIGHT])
    for row in range(HEIGHT):
        for col in range(WIDTH):
            ch = data[index][row][col]
            result[result_index] = 1 if ch != ' ' else -1
            result_index += 1
    return result

def display(pattern1, pattern2):
    index1 = 0
    index2 = 0

    for row in range(HEIGHT):
        line = ""
        for col in range(WIDTH):
            if pattern1[index1]>0:
                line += "O"
            else:
                line += " "
            index1 += 1

        line += "   ->   "

        for col in range(WIDTH):
            if pattern2[index2] >0 :
                line += "O"
            else:
                line += " "
            index2 += 1

        print(line)

def evaluate(hopfield, pattern):
    for i in range(len(pattern)):
        pattern1 = convert_pattern(pattern, i)
        hopfield.current_state = pattern1
        cycles = hopfield.run_until_stable(100)
        pattern2 = hopfield.current_state
        print("Cycles until stable(max 100): {}, result=".format(cycles))
        display(pattern1, pattern2)
        print("----------------------")

hopfield = HopfieldNetwork(WIDTH*HEIGHT)
train = TrainHopfieldHebbian(hopfield)

for i in range(len(PATTERN)):
    train.add_pattern(convert_pattern(PATTERN, i))
train.learn()

evaluate(hopfield, PATTERN)
evaluate(hopfield, PATTERN2)