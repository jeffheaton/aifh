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

* Chapter 1: Neural Network Basics, Examples:
```
CSVExample : Shows how to use CsvHelper to read a CSV
LogicExample : Simple hard-wired neural network for logic gates.
```
* Chapter 2: Self Organizing Maps, Examples:
(GUI example provided)
* Chapter 3: Hopfield & Boltzmann Machines, Examples:
```
HopfieldAssociateHebbian : Hopfield Associate - Hebbian.
HopfieldAssociateStorkey : Hopfield Associate - Storkey.
BoltzTSP : Boltzmann Traveling Salesman (TSP).
```
* Chapter 4: Feedforward Neural Networks, Examples:
```
LearnIrisAnneal : Iris ANN annealed.
```
* Chapter 5: Training & Evaluation, Examples:
* Chapter 6: Backpropagation Training, Examples:
* Chapter 7: Other Propagation Training, Examples:
* Chapter 8: NEAT, CPPN and HyperNEAT, Examples:
```
XORNEAT : Use a NEAT neural network for the XOR operator.
(GUI example included)
```
* Chapter 9: Deep Learning, Examples:
```
SimpleDBN : Simple deep belief neural network (DBNN).
```
* Chapter 10: Convolutional Neural Networks, Examples:
* Chapter 11: Pruning and Model Selection, Examples:
* Chapter 12: Dropout and Regularization, Examples:
* Chapter 13: Time Series and Recurrent Networks, Examples:
* Chapter 14: Architecting Neural Networks, Examples:
* Chapter 15: Visualization, Examples:
* Chapter 16: Modelling with Neural Networks, Examples:


Examples are under active development, more will be added soon.