AIFH Vol3  - C# Code Examples
====

This folder contains the C# examples for AIFH Volume 2.  This software was developed using Visual Studio 2012.

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

There are a total of two different project files included with the examples.  These project files are listed here.

* AIFH-Vol3 : The console examples.
* AIFH-Vol3-Core : Not executable, but contains the AI algorithms used by the other projects.

Most examples run from a single console AIFH-Vol3.exe file.  This saves considerable space when distributing the examples.
This prevents me from having to create a separate project structure for each example.  A console example is ran by 
issuing the AIFH-Vol1.exe command and following with the title of the example that you wish to run.  This can be done
either from the command line or by specifying the debug arguments when running the project.

AI For Humans, Volume 3: Deep Learning and Neural Networks
To run an example, pass its name as the first argument.

For example: 
```
AIFH-Vol3 CSVExample
```
Available Examples:

* Chapter 1: Neural Network Basics
```
CSVExample : Shows how to use CsvHelper to read a CSV
LogicExample : Simple hard-wired neural network for logic gates.
```
* Chapter 2: Self Organizing Maps
See GUI example
* Chapter 3: Hopfield & Boltzmann Machines
```
HopfieldAssociateHebbian : Hopfield Associate - Hebbian.
HopfieldAssociateStorkey : Hopfield Associate - Storkey.
BoltzTSP : Boltzmann Traveling Salesman (TSP).
```
* Chapter 4: Feedforward Neural Networks
```
LearnIrisAnneal : Iris ANN annealed.
```
* Chapter 5: Training & Evaluation
* Chapter 6: Backpropagation Training
```
LearnXORBackprop : XOR with Backpropagation.
```
* Chapter 7: Other Propagation Training
```
LearnXORRPROP : XOR with Resilient Propagation (RPROP).
```
* Chapter 8: NEAT, CPPN and HyperNEAT
```
XORNEAT : Use a NEAT neural network for the XOR operator.
LearnAutoMPGBackprop : Auto MPG Backpropagation.
LearnDigitsBackprop : MNIST Digits ANN Backpropagation.
LearnIrisBackprop : Iris ANN Backpropagation.
```
* Chapter 9: Deep Learning
```
SimpleDBN : Simple deep belief neural network (DBNN).
```
* Chapter 10: Convolutional Neural Networks
```
LearnDigitsConv : MNIST Digits Convolution Neural Network.
```
* Chapter 11: Pruning and Model Selection
```
ListSearches : List model searches: random and grid.
```
* Chapter 12: Dropout and Regularization
```
LearnDigitsDropout : MNIST Digits Dropout Neural Network.
```
* Chapter 13: Time Series and Recurrent Networks
```
PredictSunspots : Predict sunspots.
```
* Chapter 14: Architecting Neural Networks
* Chapter 15: Visualization
* Chapter 16: Modelling with Neural Networks
```
KaggleOtto : Kaggle Otto Group modeling with deep neural network.
```
Examples are under active development, more will be added soon.