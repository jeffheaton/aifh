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

## Chapter 6 Example: Error calculation

##############################################
## Define functions for MSE, RMS and SSE.
##############################################

# Calculate the error with Mean Square Error
# http://www.heatonresearch.com/wiki/Mean_Square_Error
mse <- function(a,b) {
  sum(( a - b )^2) /length(a)  
}

# Calculate the error with Root Mean Square Error
# http://www.heatonresearch.com/wiki/Root_Mean_Square_Error
rms <- function(a,b) {
  sqrt(sum(( a - b )^2) /length(a)) 
}

# Calculate the error with Sum of Squares Error
# http://www.heatonresearch.com/wiki/Root_Mean_Square_Error
sse <- function(a,b) {
  sum(( a - b )^2) /2 
}

##################################################################################
## Generate ideal data, a random data table between -5 and 5, with 3 outputs.
## Often there will be a single output, but this shows how to handle all cases.
##################################################################################

# Generate a vector of 300 random numbers between -5 and 5. Used for a 100x3 data table.
randomIdealData = runif(300,-5,5)

# Create a 2d matrix from the 1d vector
ideal <- matrix(randomIdealData,ncol=3,byrow=TRUE)

# Convert mq54ix into a data table
ideal <- as.table(ideal)

# Name the columns
colnames(ideal) <- c("out1","out2","out3")

# Name the rows, simply provide a index number for each.  Not really used.
rownames(ideal) = 1:nrow(ideal)

##############################################################################################
## Now take the random ideal data and add a small amount of noise. This creates the "actual".
## We will use error calculation methods to determine the error between the ideal and actual.
##############################################################################################
randomActualData = randomIdealData + runif(length(randomIdealData),-0.1,0.1)
actual <- matrix(randomActualData,ncol=3,byrow=TRUE)
actual <- as.table(actual)
colnames(actual) <- c("out1","out2","out3")
rownames(actual) = 1:nrow(actual)

##########################################################################
## Calculate the error for actual and ideal for all three error functions
##########################################################################

cat( "MSE error: " , mse(ideal,actual), "\n")
cat( "RMS error: " , rms(ideal,actual), "\n")
cat( "SSE error: " , sse(ideal,actual), "\n")
