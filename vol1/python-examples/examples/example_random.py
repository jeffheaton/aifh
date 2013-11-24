__author__ = 'jheaton'

import sys
import numpy as np
import matplotlib.pyplot as plt

if len(sys.argv) != 2 or sys.argv[1]=='normal':
    plt.title("Normal Random Numbers")
    x = np.random.randn(1000000)
else:
    plt.title("Uniform Random Numbers")
    x = np.random.rand(1000000)

plt.xlabel('Random Number')
plt.ylabel('Count')
plt.hist(x, 100)
plt.show()
