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

import numpy as np


class ErrorCalculation:
    def __init__(self):
        self.global_error = 0
        self.count = 0

    @staticmethod
    def rms(actual, ideal):
        return np.sqrt(np.mean((actual[:, :] - ideal[:, :]) ** 2))

    @staticmethod
    def sse(actual, ideal):
        return np.sum((actual[:, :] - ideal[:, :]) ** 2)

    @staticmethod
    def mse(actual, ideal):
        return np.mean((actual[:, :] - ideal[:, :]) ** 2)