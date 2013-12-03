#!/usr/bin/env python
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
============================================================================================================
This example shows how to chart normal and uniform random numbers.  Simply pass the parameter of "normal"
or "uniform" to the script.

"""
__author__ = 'jheaton'

import sys
import numpy as np
import matplotlib.pyplot as plt

# Determine if we are requested to plot normal or uniform.
if len(sys.argv) != 2 or sys.argv[1] == 'normal':
    plt.title("Normal Random Numbers")
    x = np.random.randn(1000000)
else:
    plt.title("Uniform Random Numbers")
    x = np.random.rand(1000000)

plt.xlabel('Random Number')
plt.ylabel('Count')
plt.hist(x, 100)
plt.show()
