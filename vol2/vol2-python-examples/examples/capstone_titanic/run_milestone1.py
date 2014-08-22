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

norm = NormalizeTitanic()
stats = TitanicStats()

norm.analyze(stats, training_path)
norm.analyze(stats, test_path)
stats.dump()

ids = []
norm.normalize(stats, training_path, ids,
                    TitanicConfig.InputNormalizeLow,
                    TitanicConfig.InputNormalizeHigh,
                    TitanicConfig.PredictSurvive,
                    TitanicConfig.PredictPerish)

# Write out the normalized file, mainly so that you can examine it.
# This file is not actually used by the program.
with open(normalize_Path, 'wb') as f:
    writer = csv.writer(f)
    writer.writerow([
        "id", "age", "sex-male", "pclass", "sibsp", "parch", "fare",
        "embarked-c", "embarked-q", "embarked-s", "name-mil", "name-nobility", "name-dr", "name-clergy"
    ])


    for idx in range(0,len(norm.result_input)):
        input = norm.result_input[idx]
        ideal = norm.result_ideal[idx]

        line = [
            ids[idx],
            input[0],
            input[1],
            input[2],
            input[3],
            input[4],
            input[5],
            input[6],
            input[7],
            input[8],
            input[9],
            input[10],
            input[11],
            input[12],
            ideal[0]]

        writer.writerow(line)
