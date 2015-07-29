import time
import numpy as np
import matplotlib.pyplot as plt

plt.axis([0, 1000, 0, 1])
plt.ion()
plt.show()

for i in range(1000):
    y = np.random.random()
    plt.scatter(i, y)
    plt.draw()
    time.sleep(0.05)