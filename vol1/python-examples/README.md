AIFH Vol1  - Python Code Examples
====
This folder contains the Python examples for AIFH Volume 1.  These examples make use of 
Python, Scipy and Numpy.  To run these examples it will be necessary to have Scipy and
Numpy installed.  Whenever possible, I used Scipy and Numpy rather than rolling my own
algorithm.  This helps performance greatly, as Scipy and Numpy both make use of native 
code.

The quickest way to get a compatible Python installation is to use Anaconda.  Anaconda
include Python, Scipy, Numpy and a few other packages useful to the scientific community.
Alternatively, you can download Python and add Numpy and Scipy yourself.  Anaconda
can be found at the following URL.

https://store.continuum.io/cshop/anaconda/

Anaconda is what I used while developing these examples.

##Data Files

There are three different data sets used by this book's examples.  All three can be found
at the UCI machine learning repository.

http://archive.ics.uci.edu/ml/

For your convenience these three data sets can be found inside of the datasets folder.
The Python examples make use of this code to find the data sets.

```python
# find the Iris data set
irisFile = os.path.dirname(os.path.realpath(__file__))
irisFile = os.path.abspath(irisFile + "../../datasets/iris.csv")

The following data sets were used.

* http://archive.ics.uci.edu/ml/datasets/Iris
* http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original)
* http://archive.ics.uci.edu/ml/datasets/Abalone
```

##Individual Examples
The individual examples are all located in the example folder.  They are listed here,
by chapter.

* Chapter 1: Introduction to AI
**example_readcsv.py
* Chapter 2: Normalizing Data
**example_normalize.py
* Chapter 3: Distance Metrics
**example_distance.py	
**example_ocr.py
* Chapter 4: Random Numbers
**example_random.py
* Chapter 5: K-Means
**example_kmeans.py	
* Chapter 6: Error Calculation
**example_error.py
* Chapter 7: Towards Machine Learning
**example_rand_polynomial.py
**example_rand_xor.py
* Chapter 8: Optimization Algorithms
**example_climb_xor.py
**example_anneal_iris.py
**example_anneal_disc_tsp.py	
**example_nm_xor.py
**example_nm_iris.py
* Chapter 9: Discrete Optimization
**example_anneal_disc_knapsack.py	
* Chapter 10: Linear Regression	
**example_linear_regression.py
**example_logit.py


##Running Examples - Command Line

##Running Examples - IDE


