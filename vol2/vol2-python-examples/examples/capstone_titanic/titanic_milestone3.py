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
import time
import csv
from titanic_milestone1 import *
from titanic_milestone2 import *

def submit(data_path, best_network, cross):
    """
    Prepare a Kaggle submission for Titanic.
    @param dataPath    The data path.
    @param bestNetwork The best network.
    @param cross       The cross validated data.
    """
    now = time.strftime("%Y%m%d-%H%M%S")

    training_path = os.path.join(data_path, TitanicConfig.TrainingFilename)
    test_path = os.path.join(data_path, TitanicConfig.TestFilename)
    score = int(cross.getScore() * 10000)
    submit_path = os.path.join(data_path, "submit-" + now + "_" + score + ".csv")
    submit_info_path = os.path.join(data_path, "submit-" + now + ".txt")

    # Create the info file
    with open(submit_info_path, 'w') as fp:
        fp.write("Crossvalidation stats:\n")
        for i in len(cross.folds):
            fold = cross.folds[i]
            fp.write("Fold #" + (i + 1) + " : Score: " + fold.score)
            fp.write("Average Score: " + cross.getScore())
            fp.write("\n")
            fp.write(str(best_network.long_term_memory))

    # Create the submit file.
    with open(submit_info_path, 'wb') as f:
        writer = csv.writer(f)
        writer.writerow(["PassengerId", "Survived"])


        norm = NormalizeTitanic()
        stats = TitanicStats()

        norm.analyze(stats, training_path)
        norm.analyze(stats, test_path)

        ids = []
        norm.normalize(stats, training_path, ids,
                    TitanicConfig.InputNormalizeLow,
                    TitanicConfig.InputNormalizeHigh,
                    TitanicConfig.PredictSurvive,
                    TitanicConfig.PredictPerish)

        for idx in range(norm.result_input):
            output = best_network.compute_regression(norm.result_input[idx])
            survived = 1 if (output[0] > 0.5) else 0

            writer.writerow([ids[idx],str(survived)])

