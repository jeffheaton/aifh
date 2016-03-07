AIFH Vol3  - Java Code Examples
====
This folder contains the Java examples for AIFH Volume 3.  This software was developed using JDK 1.7.  
To make use of this software you will need JDK 1.7 or later.

Four 3rd party JARs are also used.

The following two JAR files are used:
'net.sf.opencsv:opencsv:2.3'
'gov.nist.math:jama:1.0.3'

Additionally, the following two JAR files are needed for unit tests:

'org.hamcrest:hamcrest-all:1.3'
'junit:junit:4.10'

For performance reasons, most of the deep learning (particularly image) examples make
use of the Deeplearning4J framework.  [These examples can be found here.](https://github.com/jeffheaton/aifh/tree/master/vol3/vol3-dl4j-examples)

##Data Files

There are three different data sets used by this book's examples.  All three can be found
at the UCI machine learning repository. These files are embedded in the Java build as
resources.

http://archive.ics.uci.edu/ml/

For your convenience these three data sets can be found inside of the datasets folder.
The Python examples make use of this code to find the data sets.

##Individual Examples
The individual examples are all located in the example folder.  They are listed here,
by chapter.  I will likely be adding examples and finetuning, so please check back
if you have not downloaded a copy in awhile.

* Introduction
```
    runCSVExample - Introduction.  This example reads a CSV file
```
* Chapter 1: Neural Network Basics
```
    runLogicExample - Logic gates, hardwired.
```
* Chapter 2: Self Organizing Maps
```
    runSOMColorsExample - Self organizing map: Colors example.
    runClusterNationsExample - Cluster nations with Hexagon lattice.
```
* Chapter 3: Hopfield & Boltzmann Machines
```
    runBoltzmannTSPExample - Boltzmann Traveling Salesman (TSP).
    runHopfieldHebbianExample - Hopfield Associate - Hebbian.
    runHopfieldStorkeyExample - Hopfield Associate - Storkey.
```
* Chapter 4: Feedforward Neural Networks
```
    runIrisAnnealExample - Train an iris ANN with annealing.
```
* Chapter 5: Training & Evaluation
```
	runIrisROCExample - Iris ANN annealed ROC chart.
```
* Chapter 6: Backpropagation Training
```
	runXORBackpropExample - Chapter 6.  XOR using Backpropagation.
	runIrisBackpropExample - Chapter 6.  Iris classification using Backpropagation.
	runAutoMPGBackpropExample - Chapter 6.  MPG automobile regression using Backpropagation.
	runDigitsBackpropExample - Chapter 6.  MNIST Digit classification using Backpropagation.

```
* Chapter 7: Other Propagation Training
```
	runXORRPROPExample - Chapter 7.  XOR using RPROP.
```
* Chapter 8: NEAT, CPPN and HyperNEAT
```
    runNEATXORExample - NEAT XOR network.
    runHyperNEATBoxesExample - HyperNEAT Boxes Example.

```
* Chapter 9: Deep Learning
```
	runDBNExample, group: 'examples', description: 'Chapter 9.  Simple dbnn belief neural network.',
```
* Chapter 10: Convolutional Neural Networks

    See [Deeplearning4J Examples](https://github.com/jeffheaton/aifh/tree/master/vol3/vol3-dl4j-examples).

* Chapter 11: Pruning and Model Selection
```
	runListSearches - Compare grid and random search.
	runIrisModelSearchGrid - Grid model search.
```
* Chapter 12: Dropout and Regularization

    See [Deeplearning4J Examples](https://github.com/jeffheaton/aifh/tree/master/vol3/vol3-dl4j-examples).

* Chapter 13: Time Series and Recurrent Networks
```
	runPredictSunspots - Predict sunspots
```
* Chapter 14: Architecting Neural Networks
* Chapter 15: Visualization
```
I have not been able to find a good Java version of T-SNE, see Python example.  
```
* Chapter 16: Modeling with Neural Networks
```
	runModelKaggleOtto - Modeling, Kaggle Otto Group.
```
##Running Examples - Command Line

The easiest means for running the examples is to use the provided Gradle wrapper.  This does not require you to download
or setup any additional software.  From the directory that contains "gradlew" execute:
```
gradlew tasks
```
This should display the following (or similar):


You can now choose the example to run.  Notice the "Example Tasks" above.  You can see there is a Gradle task for every
one of the book's examples.  To run the first example, execute the following command.
```
    gradlew runCSVExample
```
Of course, remember that on UNIX you must typically prefix with ./
So the gradle command becomes.
```
    ./gradle
```
##Running Examples - IDE

Most IDE's provide the ability to import a Gradle file.  This is the easiest way to setup a project file in your IDE.
I've used this method to create all of the code in IntelliJ. Make sure that the examples have access to the 
resource files, as well as the third party JAR files.

One common error occurs when the examples cannot find the data set.  This will usually result in the following error: 

```Cannot access data set, make sure the resources are available.```

If you are getting this error using Intellij, then follow the instructions here to make sure that *.csv resources
are copied by Intellij.

http://www.jetbrains.com/idea/webhelp/resource-files.html