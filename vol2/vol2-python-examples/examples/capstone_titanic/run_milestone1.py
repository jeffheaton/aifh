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
