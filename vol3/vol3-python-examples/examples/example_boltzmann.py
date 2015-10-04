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

import math
import numpy as np
from boltzmann import *

NUM_CITIES = 4
NEURON_COUNT = NUM_CITIES * NUM_CITIES
GAMMA = 7

def sqr(x):
    return x * x

def create_cities():
    distance = np.zeros([NUM_CITIES,NUM_CITIES])

    for n1 in range(NUM_CITIES):
        for n2 in range(NUM_CITIES):
            alpha1 = float(n1 / NUM_CITIES) * 2 * math.pi
            alpha2 = float(n2 / NUM_CITIES) * 2 * math.pi
            x1 = math.cos(alpha1)
            y1 = math.sin(alpha1)
            x2 = math.cos(alpha2)
            y2 = math.sin(alpha2)
            distance[n1][n2] = math.sqrt(sqr(x1 - x2) + sqr(y1 - y2))
    return distance

def is_valid_tour(data):
    for n1 in range(NUM_CITIES):
        cities = 0
        stops = 0
        for n2 in range(NUM_CITIES):
            if data[n1 * NUM_CITIES + n2] > 0:
                if cities > 1:
                    return False
                cities += 1

            if data[n2 * NUM_CITIES + n1] > 0:
                if stops > 1:
                    return False
                stops += 1

        if (cities != 1) or (stops != 1):
            return False

    return True

def length_of_tour(distance, data):
    result = 0
    for n1 in range(NUM_CITIES):
        for n2 in range(NUM_CITIES):
            if data[((n1) % NUM_CITIES) * NUM_CITIES + n2]>0:
                break

        for n3 in range(NUM_CITIES):
            if data[((n1 + 1) % NUM_CITIES) * NUM_CITIES + n3]>0:
                break

        result += distance[n2][n3]

    return result

def display_tour(data):
    result = ""

    for n1 in range(NUM_CITIES):
        first = True
        result += "["
        for n2 in range(NUM_CITIES):
            if data[n1 * NUM_CITIES + n2] > 0:
                if first:
                    first = False
                    result += str(n2)
                else:
                    result += ", "
                    result += str(n2)

        result += "]"
        if n1 != (NUM_CITIES - 1):
            result += " -> "

    return result

def calculate_weights(distance, boltz):
    for source_tour in range(NUM_CITIES):
        for source_city in range(NUM_CITIES):
            source_index = source_tour * NUM_CITIES + source_city
            for target_tour in range(NUM_CITIES):
                for target_city in range(NUM_CITIES):
                    target_index = target_tour * NUM_CITIES + target_city
                    weight = 0

                    if source_index != target_index:
                        pred_target_tour = (NUM_CITIES - 1) if target_tour == 0 else (target_tour - 1)
                        succ_target_tour = 0 if target_tour == NUM_CITIES - 1 else (target_tour + 1)
                        if (source_tour == target_tour) or (source_city == target_city):
                            weight = -GAMMA
                        elif (source_tour == pred_target_tour) or (source_tour == succ_target_tour):
                                weight = -distance[source_city][target_city]

                    boltz.set_weight(source_index, target_index, weight)

            boltz.threshold[source_index] = -GAMMA / 2

boltz = BoltzmannMachine(NEURON_COUNT)
distance = create_cities()
calculate_weights(distance, boltz)

boltz.temperature = 100
done = False

while not done:
    boltz.establish_equilibrium()
    print("{} : {}".format(boltz.temperature, display_tour(boltz.current_state)))
    boltz.decrease_temperature(0.99)
    done = is_valid_tour(boltz.current_state)

print("Final Length: {}".format(length_of_tour(distance, boltz.current_state)))