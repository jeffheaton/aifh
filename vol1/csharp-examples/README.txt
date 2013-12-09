AIFH Vol1  - C# Code Examples
====
This folder contains the C# examples for AIFH Volume 1.  This software was developed using Visual Studio 2012.

Two 3rd party DLLs are also used. They are both included in the project via NUGET.

The following two DLLs files are used:
* Math.Net.Numerics
* CsvHelper

##Data Files

There are three different data sets used by this book's examples.  All three can be found
at the UCI machine learning repository. These files are embedded in the Java build as
resources.

http://archive.ics.uci.edu/ml/

For your convenience these three data sets can be found inside of the datasets folder.
The Python examples make use of this code to find the data sets.

##Running Examples

There are a total of four different project files included with the examples.  These project files are listed here.

* AIFH-Vol1 : The console examples.
* AIFH-Vol1-Core : Not executable, but contains the AI algorithms used by the other projects.
* AIFH-Vol1-OCR : An OCR GUI example from the distance chapter.
* AIFH-Vol1-Random : A GUI random number graphing examples that displays histograms.

Most examples run from a single console AIFH-Vol1.exe file.  This saves considerable space when distributing the examples.
This prevents me from having to create a separate project structure for each example.  A console example is ran by 
issuing the AIFH-Vol1.exe command and following with the title of the example that you wish to run.  This can be done
either from the command line or by specifying the debug arguments when running the project.

```
AI For Humans, Volume 1: Fundamental Algorithms
To run an example, pass its name as the first argument.

For example: AIFH-Vol1 CSVExample

Available Examples:

** Chapter 1: Introduction to AI, Examples:
CSVExample : Shows how to use CsvHelper to read a CSV

** Chapter 2: Normalizing Data, Examples:
NormalizeCSVExample : A simple normalization example for the Iris data set

** Chapter 3: Distance Metrics, Examples:

** Chapter 4: Random Numbers, Examples:
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
TravelingSalesmanAnneal : Travelling Salesman with Simulated Annealing

** Chapter 10: Linear Regression, Examples:
GLMExample : Use a GLM to predict the probability of breast cancer
LinearRegressionExample : Linear regression on the abalone data set
```