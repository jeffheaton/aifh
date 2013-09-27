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

## Chapter 9 Example: Discrete Optimization, Traveling Salesman
## 
## http://xkcd.com/399/
## http://en.wikipedia.org/wiki/Travelling_salesman_problem

# how many cities
cityCount <- 30

# define cities to be in a circle
ratio <- (2 * pi) / cityCount
city.x <- (cos(1:cityCount*ratio) * 10) + 10
city.y <- (sin(1:cityCount*ratio) * 10) + 10

# define an initial, random, path
path <- sample(1:cityCount, cityCount, replace=F)

# display a plot of the initial path
plot(city.x, city.y, xlim=c(0,20), ylim=c(0,20),asp = 1,xlab = "", ylab = "", axes = TRUE, main = "Traveling Salesman (before)")
arrows(city.x[path[1:(cityCount-1)]],
  city.y[path[1:(cityCount-1)]],
  city.x[path[2:cityCount]],
  city.y[path[2:cityCount]],
  angle = 10, col = "blue")

# Define a score function, this sumes the euclidean distance over the entire path
distance <- function(path) {  
  path2 <- embed(path,2)
  sqrt(sum(  (city.x[path2[,1]]-city.x[path2[,2]])^2 +  (city.y[path2[,1]]-city.y[path2[,2]])^2  ))
}

# move to neighbor solution, swap two cities on the path
moveNeighbor <- function(path) {  
  idx <- seq(2, length(path)-1)
  changepoints <- sample(idx, size = 2, replace = FALSE)
  tmp <- path[changepoints[1]]
  path[changepoints[1]] <- path[changepoints[2]]
  path[changepoints[2]] <- tmp
  path
}

# perform the simulated annealing
result <- optim(path, distance, moveNeighbor, method = "SANN",
             control = list(maxit = 200000, temp = 10, tmax=20, trace = TRUE,
                            REPORT = 500) )

# get the result path
resultPath <- result$par

# plot the result path
plot(city.x, city.y, xlim=c(0,20), ylim=c(0,20),asp = 1,xlab = "", ylab = "", axes = TRUE, main = "Traveling Salesman (After)")
arrows(city.x[resultPath[1:(cityCount-1)]],
       city.y[resultPath[1:(cityCount-1)]],
       city.x[resultPath[2:cityCount]],
       city.y[resultPath[2:cityCount]],
       angle = 10, length=0.15, col = "blue")


