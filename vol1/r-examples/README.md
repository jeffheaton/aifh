AIFH Vol1  - R Code Examples
====
This folder contains the Java examples for AIFH Volume 1.  To run one of the R examples, from
the command line, use the following command.  This would run the readCSV example.
```
r CMD BATCH ./readCSV.R
```
The output from the R example will be written to a *.Rout file, such as the following.
```
cat readCSV.Rout
```
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

```
 Directory of C:\aifh\vol1\r-examples\ch1

10/25/2013  11:18 AM             1,079 readCSV.R

 Directory of C:\Users\Jeff\projects\aifh\vol1\r-examples\ch2

10/25/2013  11:18 AM             2,477 denormalizeCSV.R
10/25/2013  11:18 AM             4,769 iris.csv
10/25/2013  11:18 AM             2,685 normalizeCSV.R

 Directory of C:\aifh\vol1\r-examples\ch3

10/25/2013  11:18 AM             1,779 distance.R

 Directory of C:\aifh\vol1\r-examples\ch4

10/25/2013  11:18 AM             2,471 random.R

 Directory of C:\aifh\vol1\r-examples\ch5

10/25/2013  11:18 AM             2,195 kmeans.R

 Directory of C:\aifh\vol1\r-examples\ch6

10/25/2013  11:18 AM             3,434 errorcalc.R

 Directory of C:\aifh\vol1\r-examples\ch7

10/25/2013  11:18 AM             3,563 learnPoly.R

 Directory of C:\aifh\vol1\r-examples\ch8

10/25/2013  11:18 AM             3,516 learnPolyAnneal.R
10/25/2013  11:18 AM             3,352 learnPolyNM.R

 Directory of C:\aifh\vol1\r-examples\ch9

10/25/2013  11:18 AM             2,544 knapsack.R
10/25/2013  11:18 AM             2,944 tsp.R

 Directory of C:\aifh\vol1\r-examples\ch10

10/25/2013  11:18 AM             1,308 multiLinearRegression.R
11/11/2013  05:09 PM               128 tempConv.R
```

##Running Examples - IDE

R Studio is a very helpful IDE for running R programs.  I used R Studio while developing
these examples.  R Studio can be found at the following URL.

http://www.rstudio.com/