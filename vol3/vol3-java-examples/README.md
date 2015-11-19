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

##Data Files

There are three different data sets used by this book's examples.  All three can be found
at the UCI machine learning repository. These files are embedded in the Java build as
resources.

http://archive.ics.uci.edu/ml/

For your convenience these three data sets can be found inside of the datasets folder.
The Python examples make use of this code to find the data sets.

##Individual Examples
The individual examples are all located in the example folder.  They are listed here,
by chapter.

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
* Chapter 5: Training & Evaluation
* Chapter 6: Backpropagation Training
* Chapter 7: Other Propagation Training
* Chapter 8: NEAT, CPPN and HyperNEAT
* Chapter 9: Deep Learning
```
runDBNExample, group: 'examples', description: 'Chapter 9.  Simple dbnn belief neural network.',
```
* Chapter 10: Convolutional Neural Networks
* Chapter 11: Pruning and Model Selection
* Chapter 12: Dropout and Regularization
* Chapter 13: Time Series and Recurrent Networks
* Chapter 14: Architecting Neural Networks
* Chapter 15: Visualization
* Chapter 16: Modelling with Neural Networks

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