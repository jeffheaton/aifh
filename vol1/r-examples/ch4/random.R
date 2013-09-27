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

## Chapter 4 Example: Random numbers

## R uses Mersenne twister as the default random number generator.
## http://en.wikipedia.org/wiki/Mersenne_twister

## There are two types of random number numbers you will typically generate.  These
## are uniform and normal random numbers.  

#############################
## Uniform random numbers  
#############################

# If you've delt with random numbers in other programming languages, you most likely made
# use of uniform random numbers.  Uniform random numbers in R are created with the runif
# function.

# The following function creates 1 random number in the range 0 to 100.
runif(1,0,100)

# Uniform numbers give all numbers in the specified range an equal probability of selection.
# This can be verified with a graph.

hist(runif(10000,0,100))

# The above graph plots 10000 random numbers.  The bars should be mostly the same hight, with
# no clear pattern.

#############################
## Normal random numbers  
#############################

# Normally distributed random numbers cluster around zero and follow the normal distribution.
# http://en.wikipedia.org/wiki/Normal_distribution

# The following function creates 1 normally distributed random number.
rnorm(1)

# You can see a random normal distribution with the following command.

hist(rnorm(10000))

# The above graph plots 10000 random numbers.  The bars should be mostly the same hight, with
# no clear pattern.


rnorm(rnorm(1000000))