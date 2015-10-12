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

import numpy as np
from window import *

# Create a simple 3-column dataset.  This will hold the values:
# [1, 10, 100]
# [2, 20, 200]
# ...
# [10, 100, 1000]

raw_data = []
for i in range(1,11):
    raw_data.append([i,i*10,i*100])

raw_data = np.array(raw_data)
result_x, result_y = encode_timeseries_window(raw_data, 3, 1, [True, True, True], [False, False, True])

result_x = np.array(result_x)
result_y = np.array(result_y)

for x,y in zip(result_x, result_y):
    print("{} --> {}".format(x,y))