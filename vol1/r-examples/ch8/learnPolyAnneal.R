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

## Chapter 8 Example: Optimization, Learn Polynomial w/ Simulated annealing
## http://en.wikipedia.org/wiki/Polynomial

# Calculate the error with Mean Square Error
# http://www.heatonresearch.com/wiki/Mean_Square_Error
mse <- function(a,b) {
  sum(( a - b )^2) /length(a)  
}

##################################################################################
## Generate the input data for training.  We use the x integer values of 
## -50 to 50 to train the polynomial over.
##################################################################################

# Generate input data range
inputData = -50:50

# Create a 2d matrix from the 1d vector
input <- matrix(inputData,ncol=1,byrow=TRUE)

# Convert into a data table
input <- as.table(input)

# Name the columns
colnames(input) <- c("x")

# Name the rows, simply provide a index number for each.  Not really used.
rownames(input) = 1:nrow(input)

##################################################################################
## Generate the ideal data for training.  We calculate the y value of the polynomial
## for each of the input values from the previous step.
##################################################################################

# Generate input data range (2x^2 + 4x + 6)
idealData = (2*(inputData^2)) + (4*inputData) + 6

# Create a 2d matrix from the 1d vector
ideal <- matrix(idealData,ncol=1,byrow=TRUE)

# Convert into a data table
ideal <- as.table(ideal)

# Name the columns
colnames(ideal) <- c("y")

# Name the rows, simply provide a index number for each.  Not really used.
rownames(ideal) = 1:nrow(ideal)

# Calculate the polynomial from the coefficient
calcPolynomial <- function(coef)
{
  actual <- (coef[1]*(input[,1]^2)) + (input[,1]*coef[2]) + coef[3]
  actual <- as.table(matrix(actual,ncol=1,byrow=TRUE))
  rownames(actual) = 1:nrow(actual)
  colnames(actual) <- c("y")
  actual
}

# Score the polynomial
score <- function(coef) {
  actual <-calcPolynomial(coef)
  mse(actual,ideal)
}

# move to neighbor solution, randomly change coef
moveNeighbor <- function(coef) { 
  idx <- sample(1:length(coef), replace = TRUE)
  coef[idx] = coef[idx] + runif(1,-0.01,0.01)
  coef
}

# simulated annealing
result <- optim(rep(0,3), score, moveNeighbor, method = "SANN",
                control = list(maxit = 10000, temp = 10, trace = TRUE,
                               REPORT = 500) )
# get the result coef
coef <- result$par

coef
cat( coef[1],"x^2 + ", coef[2], "x + ", coef[3] ) 
