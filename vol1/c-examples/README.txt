AIFH Vol1  - C/C++ Code Examples
====
This folder contains the C/C++ examples for AIFH Volume 1.  The header files are constructed 
to be compatible with both C and C++.  I may create a C++ specific version at some point
that uses STL and rolls all the struct's up into classes.  

Two 3rd party files are also used:
* mt19937ar.c - Mersenne Twister
* asa047.c - Nelder Mead (with small modifications)

##Data Files

There are three different data sets used by this book's examples.  All three can be found
at the UCI machine learning repository. These files are embedded in the Java build as
resources.

http://archive.ics.uci.edu/ml/

For your convenience these three data sets can be found inside of the datasets folder.
The Python examples make use of this code to find the data sets.

##Running Examples

The examples were tested with both Visual C++, as well as GCC.  If you are using 
Visual C++, you should make use of the included ```AIFH-VOL1.sln``` file.  If you are 
using UNIX, you should use ```Makefile```.

To compile the UNIX examples issue the following command.

```
make
```

This assumes you have gcc and make installed.    

Both examples include one file, named ```AIFH-VOL1.c``` that routes uses a command line
parameter and routes you to the correct example.  You should provide a command parameter 
that specifies the desired example. For example, to run the CSV example in UNIX, use:

```
./aifh-vol1 CSVExample
``` 

For Visual C++, set the debugger argument to CSVExample, or whatever example you choose.

The complete list of examples is given by running without a parameter.  You can see this
output here.

```
Usage:
./aifh-vol1 [-pause] ExampleName [arg1] [args2] ...

Where ExampleName is one of:
** Chapter 1: Introduction to AI, Examples:
CSVExample : Shows how to use CsvHelper to read a CSV

** Chapter 2: Normalizing Data, Examples:
NormalizeCSVExample : A simple normalization example for the Iris data set
SimpleNormalize : Perform a few very basic normalizations
Analyze : Analyze the Iris data set

** Chapter 3: Distance Metrics, Examples:
Distance : See various distance metrics

** Chapter 4: Random Numbers, Examples:
Random : Compare PRNG's and distributions
EvaluatePI : Approximate PI by Monte Carlo

** Chapter 5: K-Means, Examples:
PerformCluster : Attempt to cluster the iris data set

** Chapter 6: Error Calculation, Examples:
EvaluateErrors : Evaluate several error calculation methods

** Chapter 7: Towards Machine Learning, Examples:
LearnIris : Learn Iris data using RBF network & Greedy Random algorithm
LearnPolynomial : Learn a simple polynomial with the Greedy Random algorithm
LearnXOR : Learn the XOR function with a RBF Network trained by Greedy Random

** Chapter 8: Optimization Algorithms, Examples:
LearnIrisAnneal : Learn the Iris data set w/RBF net & annealing
LearnIrisClimb : Learn the Iris data set w/RBF net & hill climbing
LearnIrisNelderMead : Learn the Iris data set w/RBF net & Nelder Mead

** Chapter 9: Discrete Optimization, Examples:
KnapsackAnneal : Knapsack optimization with Simulated Annealing
TravelingSalesmanAnneal : Traveling Salesman with Simulated Annealing

** Chapter 10: Linear Regression, Examples:
GLMExample : Use a GLM to predict the probability of breast cancer
LinearRegressionExample : Linear regression on the abalone data set
```

##Running Examples (cmake)
A cmake project file is also included.  The cmake utility can be used to generate a 
variety of project/makefiles for various operating systems.  For more information on
cmake, refer to:

http://www.cmake.org/