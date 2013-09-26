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

## Chapter 2 Example: Normalize a CSV file

## first load the iris data set
irisdata <- read.csv(file="iris.csv",head=TRUE,sep=",")

#######################################################
## Normalize columns 1, 2, 3 & 4 to the range -1 to 1 #
#######################################################
inputData <- irisdata[1:4]

## Subtract every value from its column minimum.
norm <- sweep(inputData, 2, apply(inputData, 2, min)) 

## Divide every value (from the last step) by the column maximum
norm <- sweep(norm, 2, apply(inputData, 2, max), "/") 

## Multiply every value (from the last step) by 2 and subtract 1
norm <- 2*norm - 1

## Display the results
print("Iris data set input normalized between -1 and 1")
norm

# Write to a file, used for denormalize example
write.csv(norm, "iris_norm1.csv", row.names=FALSE)

#######################################################
## Normalize columns 1, 2, 3 & 4 to the range 0 to 1 #
#######################################################
inputData <- irisdata[1:4]

## Subtract every value from its column minimum.
norm <- sweep(inputData, 2, apply(inputData, 2, min)) 

## Divide every value (from the last step) by the column maximum
norm <- sweep(norm, 2, apply(inputData, 2, max) - apply(inputData, 2, min), "/") 

## Display the results
print("Iris data set input normalized between 0 and 1")
norm

# Write to a file, used for denormalize example
write.csv(norm, "iris_norm2.csv", row.names=FALSE)

######################################
## Normalize the species to one-of-n #
######################################

print("Iris data set species normalized with one-of-n")
library(nnet)
class.ind(irisdata$species)

