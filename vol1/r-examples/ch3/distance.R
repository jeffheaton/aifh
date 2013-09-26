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

## Chapter 3 Example: Distance calculation

## Define a function for Euclidean distance
## http://en.wikipedia.org/wiki/Euclidean_distance

dist.euclidean <- function(x1,x2) {
  sqrt(sum((x1 - x2) ^ 2))
} 

## Define a function for Manhattan distance
## http://en.wikipedia.org/wiki/Taxicab_geometry

dist.manhattan <- function(x1,x2) {
  sum(abs(x1 - x2))
} 

## Define a function for Chebyshev distance
## http://en.wikipedia.org/wiki/Chebyshev_distance

dist.chebyshev <- function(x1,x2) {
  max(abs(x1 - x2))
} 

## Create some vectors to calculate distances
vec1 <- c( 0,0,0,0 )
vec2 <- c( 10,5,3,8 )
vec3 <- c( 2,4,5,7 )

dist.euclidean(vec1,vec3)
dist.euclidean(vec2,vec3)

dist.manhattan(vec1,vec3)
dist.manhattan(vec2,vec3)

dist.chebyshev(vec1,vec3)
dist.chebyshev(vec2,vec3)
