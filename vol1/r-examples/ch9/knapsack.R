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

## Chapter 9 Example: Discrete Optimization, The Knapsack Problem
## http://en.wikipedia.org/wiki/Knapsack_problem
## http://xkcd.com/287/

# the menu
menu <- c(2.15, 2.75, 3.35, 3.55, 4.20, 5.80)

# the total amount we want to spend
maxCost <- 15.05

# score function
score <- function(contents) {
  result <- maxCost - sum(menu[1:length(menu)]*contents[1:length(menu)])
  if( result<0 ) {
    result <- .Machine$double.maxx
  }
  result
}

# move to neighbor solution, add or remove items so long as under max cost
moveNeighbor <- function(contents) { 
  repeat {
    currentSum <- sum(menu[1:length(menu)]*contents[1:length(menu)])
    
    # randomly either add or remove, try to remove more often because add is always successful
    if( runif(1,0,1)>0.25 )
    {
      # remove an item
      target = sample(1:length(menu),1)
      if( contents[target]>0 ) 
      {
        contents[target] = contents[target] - 1
      }
    }
    else 
    {
      # add an item
      target = sample(1:length(menu),1)
      contents[target] = contents[target] + 1  
    }
    
    # we are done if below max cost

    if( sum(menu*contents)<maxCost ) {
      break
    }
  }
  contents
}

# simulated annealing
result <- optim(rep(0,length(menu)), score, moveNeighbor, method = "SANN",
                control = list(maxit = 10000, temp = 10, trace = TRUE,
                               REPORT = 500) )


# get the result quantities
qty <- result$par

# display results
cbind(menu,qty,menu*qty)
cat("Total bill: " , sum(menu*qty) )
