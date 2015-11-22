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

** Chapter 1: Introduction to AI, Examples:
```
CSVExample : Shows how to use CsvHelper to read a CSV
LogicExample : Logic gates, hardwired.
```
Examples are under active development, more will be added soon.


```