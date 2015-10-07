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

class SelfOrganizingMap:
    """
    The weights of the output neurons base on the input from the input
    neurons.
    """
    def __init__(self, input_count, output_count):
        """
        The constructor.
        :param input_count: Number of input neurons
        :param output_count: Number of output neurons
        :return:
        """
        self.input_count = input_count
        self.output_count = output_count
        self.weights = np.zeros([self.output_count, self.input_count])
