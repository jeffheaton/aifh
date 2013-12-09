AIFH Vol1  - Java Code Examples
====
This folder contains the Java examples for AIFH Volume 1.  This software was developed using JDK 1.7.  
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

* Chapter 1: Introduction to AI
```
    runCSVExample - Chapter 1.  This example reads a CSV file
```
* Chapter 2: Normalizing Data
```
    runNormalizeCSVExample - Chapter 2:  This example normalizes a CSV file
```
* Chapter 3: Distance Metrics
```	
    runOCRExample - Chapter 3:  This example performs basic OCR
```
* Chapter 4: Random Numbers
```
    runEvaluatePI - Chapter 4:  This example approximates Pi using Monte Carlo
    runEvaluateRandom - Chapter 4:  This example approximates Pi using Monte Carlo
```
* Chapter 5: K-Means
```
    runPerformCluster - Chapter 5:  This example performs KMeans cluster
```
* Chapter 6: Error Calculation
```
    runEvaluateErrors - Chapter 6:  This example compares several error metrics
```
* Chapter 7: Towards Machine Learning
```
    runLearnIris - Chapter 7:  This example learns Iris w/ RBF network & Greedy Random
    runLearnPolynomial - Chapter 7:  This example learns a Polynomial w/ Greedy Random
    runLearnXOR - Chapter 7:  This example learns the XOR function w/ RBF network & Greedy Random
```
* Chapter 8: Optimization Algorithms
```
    runLearnIrisAnneal - Chapter 8:  This example learns the Iris dataset w/ RBF network & Simulated Annealing
    runLearnIrisClimb - Chapter 8:  This example learns the Iris dataset w/ RBF network & Hill Climbing
    runLearnIrisNelderMead - Chapter 8:  This example learns the Iris dataset w/ RBF network & Nelder Mead
```
* Chapter 9: Discrete Optimization
```
    runKnapsackAnneal - Chapter 9:  This example optimizes the Knapsack problem with Simulated Annealing
    runTravelingSalesmanAnneal - Chapter 9:  This example optimizes the Travelling Salesman problem with Simulated Annealing
```
* Chapter 10: Linear Regression	
```
    runGLMExample - Chapter 10:  This uses a GLM to predict breast cancer
    runLinearRegressionExample - Chapter 10:  This uses linear regression for the Iris data set
```

##Running Examples - Command Line

The easiest means for running the examples is to use the provided Gradle wrapper.  This does not require you to download
or setup any additional software.  From the directory that contains "gradlew" execute:
```
gradlew tasks
```
This should display the following (or similar):
```
    Jeffs-MacBook-Pro:java-examples jheaton$ ./gradlew tasks
    :tasks

    ------------------------------------------------------------
    All tasks runnable from root project
    ------------------------------------------------------------

    Application tasks
    -----------------
    distTar - Bundles the project as a JVM application with libs and OS specific scripts.
    distZip - Bundles the project as a JVM application with libs and OS specific scripts.
    installApp - Installs the project as a JVM application along with libs and OS specific scripts.
    run - Runs this project as a JVM application

    Build tasks
    -----------
    assemble - Assembles the outputs of this project.
    build - Assembles and tests this project.
    buildDependents - Assembles and tests this project and all projects that depend on it.
    buildNeeded - Assembles and tests this project and all projects it depends on.
    classes - Assembles the main classes.
    clean - Deletes the build directory.
    jar - Assembles a jar archive containing the main classes.
    testClasses - Assembles the test classes.

    Documentation tasks
    -------------------
    javadoc - Generates Javadoc API documentation for the main source code.

    Examples tasks
    --------------
    runCSVExample - Chapter 1.  This example reads a CSV file
    runEvaluateErrors - Chapter 6:  This example compares several error metrics
    runEvaluatePI - Chapter 4:  This example approximates Pi using Monte Carlo
    runEvaluateRandom - Chapter 4:  This example approximates Pi using Monte Carlo
    runGLMExample - Chapter 10:  This uses a GLM to predict breast cancer
    runKnapsackAnneal - Chapter 9:  This example optimizes the Knapsack problem with Simulated Annealing
    runLearnIris - Chapter 7:  This example learns Iris w/ RBF network & Greedy Random
    runLearnIrisAnneal - Chapter 8:  This example learns the Iris dataset w/ RBF network & Simulated Annealing
    runLearnIrisClimb - Chapter 8:  This example learns the Iris dataset w/ RBF network & Hill Climbing
    runLearnIrisNelderMead - Chapter 8:  This example learns the Iris dataset w/ RBF network & Nelder Mead
    runLearnPolynomial - Chapter 7:  This example learns a Polynomial w/ Greedy Random
    runLearnXOR - Chapter 7:  This example learns the XOR function w/ RBF network & Greedy Random
    runLinearRegressionExample - Chapter 10:  This uses linear regression for the Iris data set
    runNormalizeCSVExample - Chapter 2:  This example normalizes a CSV file
    runOCRExample - Chapter 3:  This example performs basic OCR
    runPerformCluster - Chapter 5:  This example performs KMeans cluster
    runTravelingSalesmanAnneal - Chapter 9:  This example optimizes the Travelling Salesman problem with Simulated Annealing

    Help tasks
    ----------
    dependencies - Displays all dependencies declared in root project 'java-examples'.
    dependencyInsight - Displays the insight into a specific dependency in root project 'java-examples'.
    help - Displays a help message
    projects - Displays the sub-projects of root project 'java-examples'.
    properties - Displays the properties of root project 'java-examples'.
    tasks - Displays the tasks runnable from root project 'java-examples' (some of the displayed tasks may belong to subprojects).

    Verification tasks
    ------------------
    check - Runs all checks.
    test - Runs the unit tests.

    Other tasks
    -----------
    wrapper

    Rules
    -----
    Pattern: build<ConfigurationName>: Assembles the artifacts of a configuration.
    Pattern: upload<ConfigurationName>: Assembles and uploads the artifacts belonging to a configuration.
    Pattern: clean<TaskName>: Cleans the output files of a task.

    To see all tasks and more detail, run with --all.

    BUILD SUCCESSFUL

    Total time: 2.781 secs
    Jeffs-MacBook-Pro:java-examples jheaton$
```

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