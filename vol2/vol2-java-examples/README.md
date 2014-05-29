AIFH Vol2  - Java Code Examples
====
This folder contains the Java examples for AIFH Volume 2.  This software was developed using JDK 1.7.  
To make use of this software you will need JDK 1.7 or later.  Examples have been tested with JDK 1.8.

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

* Chapter 1: Population and Scoring
```
runTournamentCompare - Chapter 1.  Compare selection methods
```

* Chapter 2: Crossover and Mutation 
```
runCrossover - Chapter 2.  Crossover example
runMutate - Chapter 2.  Mutate example
```

* Chapter 3: Genetic Algorithms  
```
runGAIris - Chapter 3.  Model the iris data set with a genetic algorithm (single species)
runGATSP - Chapter 3.  Use a genetic algorithm (GA) for the Travelling Salesman Problem (TSP)
```

* Chapter 4: Genetic Programming 
```
runFindEquation - Chapter 4.  Genetic programming
```

* Chapter 5: Speciation 
```
runGASpeciesIris - Chapter 5.  Model the iris data set with a genetic algorithm (multi species)
```

* Chapter 6: Particle Swarm Optimization 
```
runFlock - Chapter 6.  Flocking
runIrisPSO - Chapter 6.  Model the iris data set with particle swarm optimization(PSO)
```

* Chapter 7: Ant Colony Optimization 
```
runACOTSP - Chapter 7.  Use a ant colony optimization(ACO) for the Travelling Salesman Problem (TSP)
runIrisACO - Chapter 7.  Model the iris data set with ant colony optimization (ACO)
```

* Chapter 8: Cellular Automation
```
runConway - Chapter 8.  Run Conways game of life.
runECA - Chapter 8.  Run an Elementary cellular automation.
runMergelife - Chapter 8.  Run the merge life explorer.
```

* Chapter 9: Artificial Life
```
runCapstonePlants1 - Chapter 9. Plants Capstone Project. Milestone 1
runCapstonePlants2 - Chapter 9. Plants Capstone Project. Milestone 2
runCapstonePlants3 - Chapter 9. Plants Capstone Project. Milestone 3
```

* Chapter 10: Modeling Problems
```
runCapstoneTitanic1 - Chapter 10. Titanic Capstone Project. Milestone 1
runCapstoneTitanic2 - Chapter 10. Titanic Capstone Project. Milestone 2
runCapstoneTitanic3 - Chapter 10. Titanic Capstone Project. Milestone 3
```
Note, for Titanic, you must specify your data directory.  For example:
```
./gradlew runCapstoneTitanic1 -Pdata_path="/users/jheaton/data/titanic"
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