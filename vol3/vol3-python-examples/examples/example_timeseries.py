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

def encode_timeseries_window(source, lag_size, include_fields, lead_size):
    result = []

    idx = 0
    row = []



# Create a simple 3-column dataset.  This will hold the values:
# [1, 10, 100]
# [2, 20, 200]
# ...
# [10, 100, 1000]

raw_data = []
for i in range(1,11):
    raw_data.append([i,i*10,i*100])

raw_data = np.array(raw_data)
print(raw_data)