## Artificial Intelligence for Humans
## Volume 1: Fundamental Algorithms
## R Version
## http://www.aifh.org
## http://www.jeffheaton.com
##
## Code repository:
## https://github.com/jeffheaton/aifh
##
## Copyright 2013 by Jeff Heaton
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
## For more information on Heaton Research copyrights, licenses
## and trademarks visit:
## http://www.heatonresearch.com/copyright

## Chapter 5 Example: Random numbers

## first load the iris data set
irisdata <- read.csv(file="iris.csv",head=TRUE,sep=",")

## perform the kmeans into 3 clusters, max iterations of 1000
iris.kmeans <- kmeans(irisdata[, -5], 3, iter.max = 1000)

## display the kmeans cluster as a table
table(irisdata[, 5], iris.kmeans$cluster)

# The iris data is 4-dimension.  To display that in a chart we must scale the
# dimensions down to just 2.
iris.dist <- dist(iris[, -5])
iris.mds <- cmdscale(iris.dist)

# Ideal species assignments will be characters (from the iris data)
ideal.chars <- c("*", "o", "+")[as.integer(iris$Species)]

# Actual cluster assignments will be colors (from the kmeans cluster)
actual.colors <- rainbow(3)[iris.kmeans$cluster]

# You now plot
plot(iris.mds, col = actual.colors, pch = ideal.chars, xlab = "X", ylab = "Y")

# You will notice items on the graph.  Their char type shows their real (ideal) species.
# The colors shows what cluster kMeans put them into.  You will notice that most errors
# occur right at the border between the two clusters on the right of the graph.  That is
# because there is no clearly defined border between them for kmeans to figure out on its 
# own.
