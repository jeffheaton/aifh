__author__ = 'jheaton'

import os
import sys
import numpy as np
import pandas as pd
import statsmodels.api as sm

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from normalize import Normalize


# find the Wisconsin breast cancer data set
dataFile = os.path.dirname(os.path.realpath(__file__))
dataFile = os.path.abspath(dataFile + "../../datasets/breast-cancer-wisconsin.csv")

# Normalize the Wisconsin file.

norm = Normalize()
data_file_work = norm.load_csv(dataFile)
norm.delete_unknowns(data_file_work)
norm.col_delete(data_file_work,0)
norm.col_replace(data_file_work, 9, 4, 1, 0)

for i in xrange(0,9):
    norm.make_col_numeric(data_file_work,i)

print(data_file_work)

#training = np.array(data_file_work)
#training_input = training[:, 0:9]
#training_ideal = training[:, 9:10]

#df = pd.read_csv(dataFile)
#df = df[df.line_race != 0]

df = pd.DataFrame(data_file_work)
df.columns = ["clump_thickness","size_uniformity","shape_uniformity","marginal_adhesion","epithelial_size","bare_nucleoli","bland_chromatin","normal_nucleoli","mitoses","class"]

print(df)

train_cols = df.columns[0:9]
print(train_cols)

logit = sm.Logit(df['class'], df[train_cols])

# fit the model
result = logit.fit()

print(result.summary())