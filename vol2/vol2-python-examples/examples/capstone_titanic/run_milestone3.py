#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 2: Nature-Inspired Algorithms
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com

    Code repository:
    https://github.com/jeffheaton/aifh

    Copyright 2014 by Jeff Heaton

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
import csv

from titanic_milestone1 import *
from titanic_milestone2 import *
from titanic_milestone3 import *

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from rbf_network import *
from pso import *

# Find the data dir
if len(sys.argv) != 2:
    print("Please call this program with a single parameter that specifies your data directory.")
    sys.exit(1)
else:
    filename = sys.argv[1]

data_path = filename
training_path = os.path.join(data_path, TitanicConfig.TrainingFilename)
test_path = os.path.join(data_path, TitanicConfig.TestFilename)
normalize_Path = os.path.join(data_path, TitanicConfig.NormDumpFilename)

# Fit the model.
cross = fit_titanic(training_path, test_path)

# Build submit file

submit(data_path, cross.best_network, cross)