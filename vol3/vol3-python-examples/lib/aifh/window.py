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

def encode_timeseries_window(source, lag_size, lead_size, input_fields, predict_fields):
    """
    Encode raw data to a time-series window.
    :param source: A 2D array that specifies the source to be encoded.
    :param lag_size: The number of rows uses to predict.
    :param lead_size: The number of rows to be predicted
    :param input_fields: Boolean array that specifies which columns to use for prediction.
    :param predict_fields: Boolean array that specifies which columns to predict.
    :return: A tuple that contains the x (input) & y (expected output) for training.
    """
    result_x = []
    result_y = []

    output_row_count = len(source) - (lag_size + lead_size) + 1

    for raw_index in range(output_row_count):
        encoded_x = []

        for i, use_field in enumerate(input_fields):
            if use_field:
                for j in range(lag_size):
                    encoded_x.append(source[raw_index+j][i])

        result_x.append(encoded_x)

        # Encode y (prediction)
        encoded_y = []

        for i, use_field in enumerate(predict_fields):
            if use_field:
                for j in range(lead_size):
                    encoded_y.append(source[lag_size+raw_index+j][i])

        result_y.append(encoded_y)


    return result_x, result_y
