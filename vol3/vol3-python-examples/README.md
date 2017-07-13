AIFH Vol3  - Python Code Examples
=================================

This folder contains the Python examples for AIFH Volume 3.  These examples make use of 
Python, Scipy, Theano, Lasange and Numpy.  To run these examples it will be necessary to 
have Scipy and Numpy installed.  Whenever possible, I used Scipy and Numpy rather than 
rolling my own algorithm.  This helps performance greatly, as Scipy and Numpy both make 
use of native  code.

The quickest way to get a compatible Python installation is to use Anaconda.  Anaconda
include Python, Scipy, Numpy and a few other packages useful to the scientific community.
Alternatively, you can download Python and add Numpy and Scipy yourself.  Anaconda
can be found at the following URL.

https://store.continuum.io/cshop/anaconda/

Anaconda is what I used while developing these examples.

Required Libraries
------------------

The Python programming language has some of the most advanced deep learning neural network
frameworks available of any language.  These libraries are designed for maximum 
efficiency and computational power.  Most of these frameworks can also use a GPU to 
further speed processing.  These frameworks make use of low level C/C++ code for 
maximum performance.  Unfortunately, this increases the complexity of installing these
frameworks.  The environment that I used for these examples includes:

* [Anaconda Python 3.4](https://www.continuum.io/downloads) - Anaconda is built for scientific computing and contains many packages out of the box.  
* [TensorFlow](https://www.tensorflow.org/) - Google's numeric package for deep learning that supports CPU/GPU.
* [Keras](https://keras.io/) - A deep neural network framework built upon several lower level packages, such as TensorFlow.

The above links contain information for the installation of each of these packages.  To 
make use of all neural network examples in this book you will need the above packages
installed into Python. 

Data Files
----------

There are three different data sets used by this book's examples.  All three can be found
at the UCI machine learning repository.

http://archive.ics.uci.edu/ml/

For your convenience these three data sets can be found inside of the datasets folder.
The Python examples make use of this code to find the data sets.

```python
# find the Iris data set
irisFile = os.path.dirname(os.path.realpath(__file__))
irisFile = os.path.abspath(irisFile + "../../datasets/iris.csv")
```

The following data sets were used.

```
* http://archive.ics.uci.edu/ml/datasets/Iris
* http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original)
* http://archive.ics.uci.edu/ml/datasets/Abalone
```

##Individual Examples
The individual examples are all located in the example folder.  They are listed here,
by chapter.

* Chapter 1: Neural Network Basics
```
example_logic.py: Simple logic gates.
```
* Chapter 2: Self Organizing Maps
```
example_som_colors.py: SOM color grouping.
```
* Chapter 3: Hopfield & Boltzmann Machines
```
example_boltzmann.py: Traveling Salesman Problem using Boltzmann Machines
example_hopfield.py: Hopfield Neural Networks
```
* Chapter 4: Feedforward Neural Networks
```
example_iris_anneal.py: Train a neural network on the Iris dataset with Simulated Annealing.
```
* Chapter 5: Training & Evaluation
```
example_roc.py: Plot ROC charts.
```
* Chapter 6: Backpropagation Training
```
example_iris.py: Basic classification example of the Iris dataset.
example_mnist_sigmoid.py: Using a classic sigmoid neural network with MNIST.
example_xor.py: Classic XOR neural network.
```
* Chapter 7: Other Propagation Training
* Chapter 8: NEAT, CPPN and HyperNEAT
* Chapter 9: Deep Learning
```
example_mnist_deep.py: MNIST dataset with a 4 layer neural network.
```
* Chapter 10: Convolutional Neural Networks
```
example_mnist_conv.py: Using Convolutional neural networks for the MNIST dataset.
```
* Chapter 11: Pruning and Model Selection
```
example_model_search.py: Trying several model architectures with MNIST.
```
* Chapter 12: Dropout and Regularization
```
example_mnist_drop.py: Using dropout for the MNIST dataset.
```
* Chapter 13: Time Series and Recurrent Networks
```
example_sunspots.py: Predicting sunpots.
```
* Chapter 14: Architecting Neural Networks
```
example_benchmark.py: Framework to compare different neural network techniques.
```
* Chapter 15: Visualization
```
example_tsne.py: Plot the MNIST digits with the T-SNE algorithm.
```
* Chapter 16: Modelling with Neural Networks
```
example_otto.py: The Kaggle Otto Group (tm) classification with deep learning.
```

##Running Examples - Command Line

Running an example from the command line should be as easy as using the python command
followed by the example name.

```
bootcamp:examples jheaton$ python example_error.py
Type	SSE			MSE		RMS
Small	2505		0.01	0.10
Medium	62628		0.25	0.50
Large	250515		1.00	1.00
Huge	25051524	100.21	10.01
bootcamp:examples jheaton$ 
```

Of course, make sure that Numpy and Scipy are installed.  Also make sure python is 
on the path.  Refer to the Python or Anaconda documentation for more information on
installing Python for your operating system.

##Running Examples - IDE

There are many different Python IDE's.  So long as you have Numpy and Scipy installed,
these examples should work with any of them.  I personally use Pycharm Community Edition.

http://www.jetbrains.com/pycharm/download/