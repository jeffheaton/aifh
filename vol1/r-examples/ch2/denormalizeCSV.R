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

## Chapter 2 Example: Denormalize a CSV file

## The max and min values for the iris columns are needed for 
## denormalization.  
irisdata <- read.csv(file="iris.csv",head=TRUE,sep=",")
inputData <- irisdata[1:4]
irisMin <- apply(inputData, 2, min)
irisMax <- apply(inputData, 2, max)

## load the normalized iris data set (created by normalizeCSV.R)
norm1 <- read.csv(file="iris_norm1.csv",head=TRUE,sep=",")
norm2 <- read.csv(file="iris_norm2.csv",head=TRUE,sep=",")

###########################################################
## DeNormalize columns 1, 2, 3 & 4 from the range -1 to 1 #
###########################################################

## Multiply every value (from the last step) by 2 and subtract 1
denorm1 <- (norm1+1)/2

## Divide every value (from the last step) by the column maximum
denorm1 <- sweep(denorm1, 2, irisMax, "*") 

## Subtract every value from its column minimum.
denorm1 <- sweep(denorm1, 2, irisMin, "+") 

print("Iris -1 to 1 data denormalized")
denorm1

##########################################################
## DeNormalize columns 1, 2, 3 & 4 from the range 0 to 1 #
##########################################################

## Divide every value (from the last step) by the column maximum
denorm2 <- sweep(norm2, 2, irisMax - irisMin, "*") 

## Subtract every value from its column minimum.
denorm2 <- sweep(denorm2, 2, irisMin, "+") 

## Display the results
print("Iris data set input denormalized from between 0 and 1")
denorm2
